import strutils
import strformat
import algorithm
import tables
import stacks

import llvm/llvm

import common/[types, customerrors]
import ir/[opcodes, variables]

const GLOBAL_LEVEL = 0

type
    LLVMBackend* = ref object
        context: ContextRef
        module: ModuleRef
        builder: BuilderRef
        types: TableRef[types.TypeKind, llvm.TypeRef]
        variables: TableRef[int, llvm.ValueRef]
        stack: Stack[llvm.ValueRef]
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
                variables: newTable[int, llvm.ValueRef](),
                stack: Stack[llvm.ValueRef](),
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

proc createVariable(backend: LLVMBackend, index: int, typ: llvm.TypeRef): llvm.ValueRef =
    let pre = backend.builder.getInsertBlock()
    let llvmFunc = pre.getBasicBlockParent()
    let entryBlock = llvmFunc.getEntryBasicBlock()

    llvm.positionBuilderAtEnd(backend.builder, entryBlock)
    let variable = llvm.buildAlloca(backend.builder, typ, intToStr(index))
    llvm.positionBuilderAtEnd(backend.builder, pre)

    return variable

proc createGlobal(backend: LLVMBackend, index: int, typ: llvm.TypeRef): llvm.ValueRef =
    llvm.addGlobal(backend.module, typ, intToStr(index))

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

proc load(backend: LLVMBackend, variable: llvm.ValueRef): llvm.ValueRef =
    if backend.isGlobalVariable(variable):
        variable.getInitializer()
    else:
        llvm.buildLoad(backend.builder, variable, "")

proc store(backend: LLVMBackend, variable: llvm.ValueRef, value: llvm.ValueRef) =
    if backend.isGlobalVariable(variable):
        llvm.setInitializer(variable, value)
        llvm.setGlobalConstant(variable, llvm.False)
    else:
        discard llvm.buildStore(backend.builder, value, variable)

proc buildNot(backend: LLVMBackend, value: llvm.ValueRef): llvm.ValueRef =
    let cmp = llvm.buildICmp(backend.builder,
                             llvm.IntEQ,
                             value,
                             llvm.constInt(value.typeOfX(), 0, llvm.False),
                             "")
    llvm.buildZExt(backend.builder, cmp, value.typeOfX(), "")

proc build(backend: LLVMBackend, code: ByteCode, startIndex: int = 0, stopIndex: int = code.opcodes.len()) =
    template pop(): llvm.ValueRef =
        backend.stack.pop()

    template push(v: llvm.ValueRef) =
        backend.stack.push(v)

    var opcodeIndex = startIndex

    while opcodeIndex < stopIndex:
        let opcode = code.opcodes[opcodeIndex]

        template buildJump(condition: llvm.ValueRef) =
            let startBlock = backend.builder.getInsertBlock()
            let llvmFunc = startBlock.getBasicBlockParent()
            let thenBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "then")
            llvm.positionBuilderAtEnd(backend.builder, thenBlock)

            backend.build(code, startIndex=opcodeIndex + 1, stopIndex=opcode.value)
            opcodeIndex = opcode.value - 1

            # After generating 'then' we fetch the end of the block
            discard backend.builder.getInsertBlock()

            let jumpBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "jump")
            llvm.positionBuilderAtEnd(backend.builder, jumpBlock)

            # Return to the start block to add the conditional branch
            llvm.positionBuilderAtEnd(backend.builder, startBlock)
            discard llvm.buildCondBr(backend.builder, condition, thenBlock, jumpBlock)

            llvm.positionBuilderAtEnd(backend.builder, jumpBlock)

        case opcode.kind:
        of StoreVar:
            let value = pop()

            var variable = backend.variables.getOrDefault(opcode.value, nil)
            if variable == nil:
                let createVar = if backend.isGlobalScope(): createGlobal else: createVariable
                backend.variables[opcode.value] = createVar(backend, opcode.value, value.typeOfX())
                variable = backend.variables[opcode.value]

            backend.store(variable, value)

        of opcodes.Store:
            let variable = pop()
            let value = pop()
            backend.store(variable, value)

        of LoadVar:
            push(backend.variables[opcode.value])

        of opcodes.Load:
            push(backend.load(pop()))

        of LoadConst:
            let variable = code.consts[opcode.value]
            let variableType = code.consts[opcode.value].typ
            let llvmType = backend.getLLVMType(variableType) 
            let llvmValue = case variableType.kind:
            of Signed32:
                llvm.constInt(llvmType,
                              cast[culonglong](parseInt(variable.value)),
                              llvm.True)
            of Unsigned32:
                llvm.constInt(llvmType,
                              cast[culonglong](parseInt(variable.value)),
                              llvm.False)
            of Float32:
                llvm.constRealOfString(llvmType, variable.value)
            of Boolean:
                let val = if variable.value == BOOLEAN_TRUE: 1 else: 0
                llvm.constInt(llvmType, cast[culonglong](val), llvm.False)
            else:
                raise newBackendError(fmt"unsupported const: {variableType.kind}")

            push(llvmValue)

        of Compare:
            let right = pop()
            let left = pop()

            proc buildCmp(intPredicate: llvm.IntPredicate,
                          realPredicate: llvm.RealPredicate): llvm.ValueRef =
                if left.isReal():
                    return llvm.buildFCmp(backend.builder, realPredicate, left, right, "")
                else:
                    # TODO: signed / unsigned
                    return llvm.buildICmp(backend.builder, intPredicate, left, right, "")

            let condition = case opcode.compare:
            of Equal:
                buildCmp(llvm.IntEQ, llvm.RealOEQ)
            of BiggerThan:
                buildCmp(llvm.IntSGT, llvm.RealOGT)
            of BiggerThanEqual:
                buildCmp(llvm.IntSGE, llvm.RealOGE)
            of SmallerThan:
                buildCmp(llvm.IntSLT, llvm.RealOLT)
            of SmallerThanEqual:
                buildCmp(llvm.IntSLE, llvm.RealOLE)

            push(condition)

        of JumpFalse:
            let condition = pop()
            buildJump(condition)

        of JumpTrue:
            let condition = backend.buildNot(pop())
            buildJump(condition)

        of opcodes.Not:
            push(backend.buildNot(pop()))

        of opcodes.Add:
            let right = pop()
            let left = pop()

            let f = if left.isReal(): llvm.buildFAdd else: llvm.buildAdd
            push(f(backend.builder, left, right, ""))

        of Subtract:
            let right = pop()
            let left = pop()

            let f = if left.isReal(): llvm.buildFSub else: llvm.buildSub
            push(f(backend.builder, left, right, ""))

        of Multiply:
            let right = pop()
            let left = pop()

            let f = if left.isReal(): llvm.buildFMul else: llvm.buildMul
            push(f(backend.builder, left, right, ""))

        of Function:
            let name = opcode.name

            let retType = backend.getLLVMType(opcode.ret)
            var paramTypes: seq[llvm.TypeRef] = @[]

            for t in opcode.arguments:
                paramTypes.add(backend.getLLVMType(t))

            let funcType = llvm.functionType(retType, paramTypes)
            let llvmFunc = llvm.addFunction(backend.module, name, funcType)

            for i, t in opcode.arguments:
                let index = i + backend.variables.len()
                llvmFunc.getParam(cast[cuint](i)).setValueName(intToStr(index))

            let entryBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "entry")
            llvm.positionBuilderAtEnd(backend.builder, entryBlock)

            # TODO: better copy
            var oldVariables = initTable[int, llvm.ValueRef]()
            for key in backend.variables.keys():
                oldVariables[key] = backend.variables[key]

            # Now that we have created the entry block allocate the function arguments
            for i, t in opcode.arguments:
                let index = i + backend.variables.len()
                let variable = backend.createVariable(index, backend.getLLVMType(t))
                discard llvm.buildStore(backend.builder, llvmFunc.getParam(cast[cuint](i)), variable)
                backend.variables[index] = variable

            inc(backend.level)
            backend.build(opcode.code)  # TODO: LLVMVerifyFunction
            dec(backend.level)

            backend.variables.clear()
            for key in oldVariables.keys():
                backend.variables[key] = oldVariables[key]

        of opcodes.Call:
            let name = opcode.call
            let llvmFunc = llvm.getNamedFunction(backend.module, name)
            if llvmFunc == nil:
                raise newBackendError(fmt"no function named: {name}")

            var args: seq[llvm.ValueRef] = @[]

            for _ in 0..<llvmFunc.countParams():
                args.add(pop())

            if llvmFunc.countParams() > 1:
                args.reverse()

            push(llvm.buildCall(backend.builder, llvmFunc, args, ""))

        of Return:
            discard llvm.buildRet(backend.builder, pop())

        of Cast:
            let llvmValue = pop()
            let toLlvmType = backend.getLLVMType(opcode.toType)

            let leftKind = llvmValue.typeOfX().getTypeKind()
            let rightKind = toLlvmType.getTypeKind()

            template raiseUnsupportedCast() =
                raise newBackendError(fmt"unsupported cast from {leftKind} to {rightKind}")

            let variable = case leftKind:
            of llvm.IntegerTypeKind:
                case rightKind:
                of llvm.PointerTypeKind:
                    llvm.buildIntToPtr(backend.builder, llvmValue, toLlvmType, "")
                else:
                    raiseUnsupportedCast()

            of llvm.PointerTypeKind:
                case rightKind:
                of llvm.PointerTypeKind:
                    llvm.buildPointerCast(backend.builder, llvmValue, toLlvmType, "")
                of llvm.IntegerTypeKind:
                    llvm.buildPtrToInt(backend.builder, llvmValue, toLlvmType, "")
                else:
                    raiseUnsupportedCast()

            else:
                raiseUnsupportedCast()

            push(variable)

        else:
            raise newBackendError(fmt"unsupported opcode: {opcode.kind}")

        inc(opcodeIndex)

proc feed*(backend: LLVMBackend, code: ByteCode) =
    backend.build(code)
