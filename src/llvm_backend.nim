import strutils
import strformat
import tables

import llvm

import types
import ast
import tokens

type
    LLVMBackend* = ref object
        context: ContextRef
        module: ModuleRef
        builder: BuilderRef
        types: TableRef[types.TypeKind, llvm.TypeRef]
        variables: TableRef[string, llvm.ValueRef]

proc newLLVMBackend*(): LLVMBackend =
    let context = llvm.getGlobalContext()
    let module = llvm.moduleCreateWithNameInContext("module", context)
    let builder = llvm.createBuilderInContext(context)

    let types = newTable[types.TypeKind, llvm.TypeRef]()
    let int32Type = llvm.intTypeInContext(context, 32)
    let int8Type = llvm.intTypeInContext(context, 8)
    let floatType = llvm.floatTypeInContext(context)
    types[Unsigned32] = int32Type
    types[Signed32] = int32Type
    types[Float32] = floatType
    types[Boolean] = int8Type

    LLVMBackend(context: context,
                module: module,
                builder: builder,
                types: types,
                variables: newTable[string, llvm.ValueRef]())

proc optimize(backend: LLVMBackend) =
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

proc compile*(backend: LLVMBackend, outputPath: string, target: string) =
    backend.optimize()

    initializeAllAsmPrinters()
    initializeAllTargets()
    initializeAllTargetInfos()
    initializeAllTargetMCs()

    var tr: llvm.TargetRef
    discard llvm.getTargetFromTriple(target, tr.addr, nil)
    let targetMachine = llvm.createTargetMachine(tr, target, "", "",
                                                 llvm.CodeGenLevelDefault,
                                                 llvm.RelocPIC,
                                                 llvm.CodeModelDefault)
    var err: cstring
    let compilation_failed = llvm.targetMachineEmitToFile(targetMachine,
                                                          backend.module,  # TODO: LLVMVerifyModule
                                                          outputPath,
                                                          llvm.ObjectFile,
                                                          cast[cstringArray](addr(err)))

    # TODO: extract the shellcode from the object file
    if llvm.True == compilation_failed:
        raise newException(LibraryError, fmt"llvm compilation error: {err}")

proc createVariable(backend: LLVMBackend, name: string, typ: Type): llvm.ValueRef =
    let llvmType = backend.types[typ.kind]
    let pre = backend.builder.getInsertBlock()
    let llvmFunc = pre.getBasicBlockParent()
    let entryBlock = llvmFunc.getEntryBasicBlock()

    llvm.positionBuilderAtEnd(backend.builder, entryBlock)
    let variable = llvm.buildAlloca(backend.builder, llvmType, cast[cstring](name))
    llvm.positionBuilderAtEnd(backend.builder, pre)

    return variable

proc isReal(val: llvm.ValueRef): bool =
    case val.typeOfX().getTypeKind():
    of HalfTypeKind, FloatTypeKind, DoubleTypeKind, X86FP80TypeKind, FP128TypeKind, PPC_FP128TypeKind:
        true
    else:
        false

proc build(backend: LLVMBackend, expression: Expression): llvm.ValueRef =
    case expression.kind:
    of Literal:
        case expression.literalType.kind:
        of Signed32:
            return llvm.constInt(backend.types[expression.literalType.kind],
                                 cast[culonglong](parseInt(expression.literal)),
                                 llvm.True)
        of Unsigned32:
            return llvm.constInt(backend.types[expression.literalType.kind],
                                 cast[culonglong](parseInt(expression.literal)),
                                 llvm.False)
        of Float32:
            return llvm.constRealOfString(backend.types[expression.literalType.kind],
                                          cast[cstring](expression.literalType.value))
        of Boolean:
            let val = if expression.literalType.value == BOOLEAN_TRUE: 1 else: 0
            return llvm.constInt(backend.types[expression.literalType.kind],
                                 cast[culonglong](val),
                                 llvm.False)
        else:
            raise newException(LibraryError, fmt"unsupported literal: {expression.literalType.kind}")

    of Ident:
        let name = expression.value
        let variable = backend.variables[name]
        return llvm.buildLoad(backend.builder, variable, "")

    of Declaration:
        let assignExpr = expression.declExpr
        case assignExpr.kind:
        of Assign:
            let typedIdent = assignExpr.assignee
            let ident = typedIdent.ident
            let name = ident.value
            let variable = backend.createVariable(name, typedIdent.identType)
            backend.variables[name] = variable
            discard backend.build(assignExpr)
            return variable
        else:
            # in the future, we will be able to declare variables
            # without assigning to them immediately
            discard

    of Assign:
        let left = expression.assignee
        let right = expression.assignExpr
        case left.kind:
        of TypedIdent:
            let ident = left.ident
            let name = ident.value
            let variable = backend.variables[name]
            let variableInit = backend.build(right)
            return llvm.buildStore(backend.builder, variableInit, variable)
        of Ident:
            let name = left.value
            let variable = backend.variables[name]
            let variableInit = backend.build(right)
            return llvm.buildStore(backend.builder, variableInit, variable)
        else:
            raise newException(LibraryError, fmt"unsupported asignee: {expression.token.kind}")

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
            raise newException(LibraryError, fmt"unsupported binop: {expression.token.kind}")

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
        else:
            raise newException(LibraryError, fmt"unsupported unary: {expression.token.kind}")

    of FunctionDeclaration:
        let name = expression.declName

        let retType = backend.types[expression.returnType.kind]
        var paramTypes: seq[llvm.TypeRef] = @[]

        for exp in expression.declParams.expressions:
            paramTypes.add(backend.types[exp.identType.kind])

        let funcType = llvm.functionType(retType, paramTypes)
        let llvmFunc = llvm.addFunction(backend.module, cast[cstring](name), funcType)

        var i = 0
        for exp in expression.declParams.expressions:
            llvmFunc.getParam(cast[cuint](i)).setValueName(cast[cstring](exp.ident.value))
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

        let body = backend.build(expression.implementation)  # TODO: LLVMVerifyFunction

        backend.variables.clear()
        for key in backend.variables.keys():
            backend.variables[key] = oldVariables[key]

        return body

    of FunctionCall:
        let name = expression.name
        let llvmFunc = llvm.getNamedFunction(backend.module, cast[cstring](name))
        if llvmFunc == nil:
            raise newException(LibraryError, fmt"no function named: {name}")

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
        discard

    else:
        raise newException(LibraryError, fmt"unsupported expression: {expression.kind}")

proc feed*(backend: LLVMBackend, expression: Expression) =
    discard backend.build(expression)
