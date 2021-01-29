import strutils
import strformat
import tables

import llvm/llvm

import common/types
import common/customerrors
import frontend/ast
import frontend/tokens

const GLOBAL_LEVEL = 0

type
    LLVMBackend* = ref object
        context: ContextRef
        module: ModuleRef
        builder: BuilderRef
        types: TableRef[types.TypeKind, llvm.TypeRef]
        variables: TableRef[string, llvm.ValueRef]
        level: int

proc newLLVMBackend*(): LLVMBackend =
    let context = llvm.getGlobalContext()
    let module = llvm.moduleCreateWithNameInContext("module", context)
    let builder = llvm.createBuilderInContext(context)

    let types = newTable[types.TypeKind, llvm.TypeRef]()
    let voidType = llvm.voidTypeInContext(context)
    let int32Type = llvm.intTypeInContext(context, 32)
    let int8Type = llvm.intTypeInContext(context, 8)
    let floatType = llvm.floatTypeInContext(context)
    types[Void] = voidType
    types[Unsigned32] = int32Type
    types[Signed32] = int32Type
    types[Float32] = floatType
    types[Boolean] = int8Type

    LLVMBackend(context: context,
                module: module,
                builder: builder,
                types: types,
                variables: newTable[string, llvm.ValueRef](),
                level: GLOBAL_LEVEL)

proc optimize(backend: LLVMBackend) =
    # TODO: align-all-blocks=1 / align-all-functions=1
    let pmb = llvm.passManagerBuilderCreate()
    # TODO: figure out why setting opt level 2, optimizes recusrion to infinite loops
    pmb.passManagerBuilderSetOptLevel(1)
    pmb.passManagerBuilderSetSizeLevel(2)

    let fpm = backend.module.createFunctionPassManagerForModule()
    try:
        let mpm = llvm.createPassManager()
        try:
            pmb.passManagerBuilderPopulateFunctionPassManager(fpm)
            pmb.passManagerBuilderPopulateModulePassManager(mpm)

            var f = backend.module.getFirstFunction()
            while f != nil:
                discard fpm.runFunctionPassManager(f)
                f = f.getNextFunction()
            discard mpm.runPassManager(backend.module)
        finally:
            mpm.disposePassManager()
    finally:
        fpm.disposePassManager()

proc prepareCompilation(backend: LLVMBackend) =
    backend.optimize()

    initializeAllAsmPrinters()
    initializeAllTargets()
    initializeAllTargetInfos()
    initializeAllTargetMCs()

proc getTargetMachine(target: string): llvm.TargetMachineRef =
    var tr: llvm.TargetRef
    discard llvm.getTargetFromTriple(target, tr.addr, nil)
    return llvm.createTargetMachine(tr, target, "", "",
                                    llvm.CodeGenLevelDefault,
                                    llvm.RelocPIC,
                                    llvm.CodeModelDefault)

proc compile*(backend: LLVMBackend, outputPath: string, target: string, outputAsm: bool = false) =
    backend.prepareCompilation()
    let targetMachine = target.getTargetMachine()
    var err: cstring
    let fileType = if outputAsm: llvm.AssemblyFile else: llvm.ObjectFile
    let compilation_failed = llvm.targetMachineEmitToFile(targetMachine,
                                                          backend.module,  # TODO: LLVMVerifyModule
                                                          outputPath,
                                                          fileType,
                                                          cast[cstringArray](addr(err)))

    # TODO: extract the shellcode from the object file
    if llvm.True == compilation_failed:
        raise newLLVMError(fmt"llvm compilation error: {err}")

proc getLLVMType(backend: LLVMBackend, typ: Type): llvm.TypeRef =
    result = backend.types[typ.kind]
    for i in 0..<typ.ptrLevel:
        result = llvm.pointerType(result)

proc createVariable(backend: LLVMBackend, name: string, typ: Type): llvm.ValueRef =
    let pre = backend.builder.getInsertBlock()
    let llvmFunc = pre.getBasicBlockParent()
    let entryBlock = llvmFunc.getEntryBasicBlock()

    llvm.positionBuilderAtEnd(backend.builder, entryBlock)
    let variable = llvm.buildAlloca(backend.builder, backend.getLLVMType(typ), name)
    llvm.positionBuilderAtEnd(backend.builder, pre)

    return variable

proc createGlobal(backend: LLVMBackend, name: string, typ: Type): llvm.ValueRef =
    llvm.addGlobal(backend.module, backend.getLLVMType(typ), name)

proc isReal(val: llvm.ValueRef): bool =
    case val.typeOfX().getTypeKind():
    of HalfTypeKind, FloatTypeKind, DoubleTypeKind, X86FP80TypeKind, FP128TypeKind, PPC_FP128TypeKind:
        true
    else:
        false

proc isGlobalScope(backend: LLVMBackend): bool =
    backend.level == GLOBAL_LEVEL

proc isGlobalVariable(backend: LLVMBackend, val: llvm.ValueRef): bool =
    llvm.getNamedGlobal(backend.module, val.getValueName()) != nil

proc build(backend: LLVMBackend, expression: Expression): llvm.ValueRef =
    case expression.kind:
    of Literal:
        let llvmType = backend.getLLVMType(expression.literalType) 
        case expression.literalType.kind:
        of Signed32:
            return llvm.constInt(llvmType,
                                 cast[culonglong](parseInt(expression.literal)),
                                 llvm.True)
        of Unsigned32:
            return llvm.constInt(llvmType,
                                 cast[culonglong](parseInt(expression.literal)),
                                 llvm.False)
        of Float32:
            return llvm.constRealOfString(llvmType, expression.literalType.value)
        of Boolean:
            let val = if expression.literalType.value == BOOLEAN_TRUE: 1 else: 0
            return llvm.constInt(llvmType, cast[culonglong](val), llvm.False)
        else:
            raise newBackendError(fmt"unsupported literal: {expression.literalType.kind}")

    of Cast:
        let llvmValue = backend.build(expression.castExpr)
        let toLlvmType = backend.getLLVMType(expression.toType)

        let leftKind = llvmValue.typeOfX().getTypeKind()
        let rightKind = toLlvmType.getTypeKind()

        template raiseUnsupportedCast() =
            raise newBackendError(fmt"unsupported cast from {leftKind} to {rightKind}")

        case leftKind:
        of llvm.IntegerTypeKind:
            case rightKind:
            of llvm.PointerTypeKind:
                return llvm.buildIntToPtr(backend.builder, llvmValue, toLlvmType, "")
            else:
                raiseUnsupportedCast()

        of llvm.PointerTypeKind:
            case rightKind:
            of llvm.PointerTypeKind:
                return llvm.buildPointerCast(backend.builder, llvmValue, toLlvmType, "")
            of llvm.IntegerTypeKind:
                return llvm.buildPtrToInt(backend.builder, llvmValue, toLlvmType, "")
            else:
                raiseUnsupportedCast()

        else:
            raiseUnsupportedCast()

    of Ident:
        let name = expression.value
        let variable = backend.variables[name]
        if backend.isGlobalScope():
            # must be a global variable if we are in the global scope.
            # if not, the frontend has made an error.
            return variable.getInitializer()
        else:
            return llvm.buildLoad(backend.builder, variable, "")

    of Declaration:
        let assignExpr = expression.declExpr
        case assignExpr.kind:
        of Assign:
            let typedIdent = assignExpr.assignee
            let ident = typedIdent.ident
            let name = ident.value
            let createVar = if backend.isGlobalScope(): createGlobal else: createVariable
            let variable = createVar(backend, name, typedIdent.identType)
            backend.variables[name] = variable
            discard backend.build(assignExpr)
            return variable
        else:
            # in the future, we will be able to declare variables
            # without assigning to them immediately
            discard

    of Assign:
        var ptrLevel = 0
        var left = expression.assignee
        let right = expression.assignExpr
        let name = case left.kind:
            of TypedIdent:
                left.ident.value
            of Ident:
                left.value
            of Unary:
                inc(ptrLevel)
                while left.unaryExpr.kind == Unary:
                    inc(ptrLevel)
                    left = left.unaryExpr
                left.unaryExpr.value
            else:
                raise newBackendError(fmt"unsupported asignee: {expression.token.kind}")

        var variable = backend.variables[name]
        while ptrLevel > 0:
            if backend.isGlobalVariable(variable):
                variable = variable.getInitializer()
            else:
                variable = llvm.buildLoad(backend.builder, variable, "")
            dec(ptrLevel)

        let variableInit = backend.build(right)

        if backend.isGlobalVariable(variable):
            llvm.setInitializer(variable, variableInit)
            llvm.setGlobalConstant(variable, llvm.False)
        else:
            discard llvm.buildStore(backend.builder, variableInit, variable)

        return variable

    of BinOp:
        let left = backend.build(expression.left)
        let right = backend.build(expression.right)

        proc buildCmp(intPredicate: llvm.IntPredicate,
                      realPredicate: llvm.RealPredicate): llvm.ValueRef =
            if left.isReal():
                return llvm.buildFCmp(backend.builder, realPredicate, left, right, "")
            else:
                # TODO: signed / unsigned
                return llvm.buildICmp(backend.builder, intPredicate, left, right, "")

        case expression.token.kind:
        of tokens.Plus:
            let f = if left.isReal(): llvm.buildFAdd else: llvm.buildAdd
            return f(backend.builder, left, right, "")
        of tokens.Minus:
            let f = if left.isReal(): llvm.buildFSub else: llvm.buildSub
            return f(backend.builder, left, right, "")
        of tokens.Mul:
            let f = if left.isReal(): llvm.buildFMul else: llvm.buildMul
            return f(backend.builder, left, right, "")
        of tokens.SmallerThan:
            return buildCmp(llvm.IntSLT, llvm.RealOLT)
        of tokens.BiggerThan:
            return buildCmp(llvm.IntSGT, llvm.RealOGT)
        of tokens.SmallerThanEqual:
            return buildCmp(llvm.IntSLE, llvm.RealOLE)
        of tokens.BiggerThanEqual:
            return buildCmp(llvm.IntSGE, llvm.RealOGE)
        of tokens.DoubleEqual:
            return buildCmp(llvm.IntEQ, llvm.RealOEQ)
        else:
            raise newBackendError(fmt"unsupported binop: {expression.token.kind}")

    of Unary:
        case expression.token.kind:
        of Not:
            let llvmVal = backend.build(expression.unaryExpr)
            let cmp = llvm.buildICmp(backend.builder,
                                    llvm.IntEQ,
                                    llvmVal,
                                    llvm.constInt(llvmVal.typeOfX(), 0, llvm.False),
                                    "")
            return llvm.buildZExt(backend.builder, cmp, llvmVal.typeOfX(), "")
        of Ampersand:
            return backend.variables[expression.unaryExpr.value]
        of tokens.Mul:
            let llvmVal = backend.build(expression.unaryExpr)
            return llvm.buildLoad(backend.builder, llvmVal, "")
        else:
            raise newBackendError(fmt"unsupported unary: {expression.token.kind}")

    of FunctionDeclaration:
        let name = expression.declName

        let retType = backend.getLLVMType(expression.returnType)
        var paramTypes: seq[llvm.TypeRef] = @[]

        for exp in expression.declParams.expressions:
            paramTypes.add(backend.getLLVMType(exp.identType))

        let funcType = llvm.functionType(retType, paramTypes)
        let llvmFunc = llvm.addFunction(backend.module, name, funcType)

        var i = 0
        for exp in expression.declParams.expressions:
            llvmFunc.getParam(cast[cuint](i)).setValueName(exp.ident.value)
            inc(i)

        let entryBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "entry")
        llvm.positionBuilderAtEnd(backend.builder, entryBlock)

        # TODO: better copy
        var oldVariables = initTable[string, llvm.ValueRef]()
        for key in backend.variables.keys():
            oldVariables[key] = backend.variables[key]

        # Now that we have created the entry block allocate the function arguments
        i = 0
        for exp in expression.declParams.expressions:
            let name = exp.ident.value
            let variable = backend.createVariable(name, exp.identType)
            discard llvm.buildStore(backend.builder, llvmFunc.getParam(cast[cuint](i)), variable)
            backend.variables[name] = variable
            inc(i)

        inc(backend.level)
        let body = backend.build(expression.implementation)  # TODO: LLVMVerifyFunction
        dec(backend.level)

        backend.variables.clear()
        for key in oldVariables.keys():
            backend.variables[key] = oldVariables[key]

        return body

    of FunctionCall:
        let name = expression.name
        let llvmFunc = llvm.getNamedFunction(backend.module, name)
        if llvmFunc == nil:
            raise newBackendError(fmt"no function named: {name}")

        var args: seq[llvm.ValueRef] = @[]

        for param in expression.params.expressions:
            args.add(backend.build(param))

        return llvm.buildCall(backend.builder, llvmFunc, args, "")

    of Block:
        var last: llvm.ValueRef
        for exp in expression.expressions:
            last = backend.build(exp)
        return last

    of Return:
        return llvm.buildRet(backend.builder, backend.build(expression.retExpr))

    of IfThen:
        let condition = backend.build(expression.condition)

        let startBlock = backend.builder.getInsertBlock()
        let llvmFunc = startBlock.getBasicBlockParent()
        let thenBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "then")
        llvm.positionBuilderAtEnd(backend.builder, thenBlock)

        discard backend.build(expression.then)

        # After generating 'then' we fetch the end of the block
        let thenEndBlock = backend.builder.getInsertBlock()

        let afterBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "after_if")
        llvm.positionBuilderAtEnd(backend.builder, afterBlock)

        # Return to the start block to add the conditional branch
        llvm.positionBuilderAtEnd(backend.builder, startBlock)
        discard llvm.buildCondBr(backend.builder, condition, thenBlock, afterBlock)

        # Set an unconditional branch at the end of the 'then' block to the 'after_if' block
        llvm.positionBuilderAtEnd(backend.builder, thenEndBlock)
        discard llvm.buildBr(backend.builder, afterBlock)

        llvm.positionBuilderAtEnd(backend.builder, afterBlock)

    of IfElseThen:
        let ifThen = expression.ifThen
        let condition = backend.build(ifThen.condition)

        let startBlock = backend.builder.getInsertBlock()
        let llvmFunc = startBlock.getBasicBlockParent()
        let thenBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "then")
        llvm.positionBuilderAtEnd(backend.builder, thenBlock)
        discard backend.build(ifThen.then)

        # After generating 'then' we fetch the end of the block
        let thenEndBlock = backend.builder.getInsertBlock()

        let elseBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "else")
        llvm.positionBuilderAtEnd(backend.builder, elseBlock)
        discard backend.build(expression.otherwise)

        # After generating 'else' we fetch the end of the block
        let elseEndBlock = backend.builder.getInsertBlock()

        let afterBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "after_if")
        llvm.positionBuilderAtEnd(backend.builder, afterBlock)

        # TODO: Add a phi result to if statements
        # let phi = llvm.buildPhi(backend.builder, thenVal.typeOfX(), "if_result")
        # phi.addIncoming([thenVal, elseVal], [thenEndBlock, elseEndBlock])

        # Return to the start block to add the conditional branch
        llvm.positionBuilderAtEnd(backend.builder, startBlock)
        discard llvm.buildCondBr(backend.builder, condition, thenBlock, elseBlock)

        # Set an unconditional branch at the end of the blocks to the 'after_if' block
        llvm.positionBuilderAtEnd(backend.builder, thenEndBlock)
        discard llvm.buildBr(backend.builder, afterBlock)
        llvm.positionBuilderAtEnd(backend.builder, elseEndBlock)
        discard llvm.buildBr(backend.builder, afterBlock)

        llvm.positionBuilderAtEnd(backend.builder, afterBlock)

        # return phi

    of Empty:
        return nil

    else:
        raise newBackendError(fmt"unsupported expression: {expression.kind}")

proc feed*(backend: LLVMBackend, expression: Expression) =
    discard backend.build(expression)
