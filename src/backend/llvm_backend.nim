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
    LLVMVar = ref object
        llvmValue: llvm.ValueRef
        typ: types.Type

    LLVMBackend* = ref object
        context: ContextRef
        module: ModuleRef
        builder: BuilderRef
        types: TableRef[types.TypeKind, llvm.TypeRef]
        variables: TableRef[int, LLVMVar]
        functionsRetTypes: TableRef[string, types.Type]
        stack: Stack[LLVMVar]
        level: int

proc newLLVMVar(v: llvm.ValueRef, t: types.Type): LLVMVar =
    LLVMVar(llvmValue: v, typ: t)

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
                variables: newTable[int, LLVMVar](),
                functionsRetTypes: newTable[string, Type](),
                stack: Stack[LLVMVar](),
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

proc createVariable(backend: LLVMBackend, index: int, typ: Type): LLVMVar =
    let pre = backend.builder.getInsertBlock()
    let llvmFunc = pre.getBasicBlockParent()
    let entryBlock = llvmFunc.getEntryBasicBlock()

    llvm.positionBuilderAtEnd(backend.builder, entryBlock)
    let variable = llvm.buildAlloca(backend.builder, backend.getLLVMType(typ), intToStr(index))
    llvm.positionBuilderAtEnd(backend.builder, pre)

    newLLVMVar(variable, typ)

proc createGlobal(backend: LLVMBackend, index: int, typ: Type): LLVMVar =
    let global = llvm.addGlobal(backend.module, backend.getLLVMType(typ), intToStr(index))
    newLLVMVar(global, typ)

proc isReal(val: LLVMVar): bool =
    case val.typ.kind:
    of Float32:
        true
    else:
        false

proc isSigned(val: LLVMVar): bool =
    case val.typ.kind:
    of Signed32:
        true
    else:
        false

proc isGlobalScope(backend: LLVMBackend): bool =
    backend.level == GLOBAL_LEVEL

proc isGlobalVariable(backend: LLVMBackend, val: LLVMVar): bool =
    llvm.getNamedGlobal(backend.module, val.llvmValue.getValueName()) != nil

proc load(backend: LLVMBackend, variable: LLVMVar): LLVMVar =
    let v = if backend.isGlobalVariable(variable):
        variable.llvmValue.getInitializer()
    else:
        llvm.buildLoad(backend.builder, variable.llvmValue, "")
    newLLVMVar(v, variable.typ)

proc store(backend: LLVMBackend, variable: LLVMVar, value: LLVMVar) =
    if backend.isGlobalVariable(variable):
        llvm.setInitializer(variable.llvmValue, value.llvmValue)
        llvm.setGlobalConstant(variable.llvmValue, llvm.False)
    else:
        discard llvm.buildStore(backend.builder, value.llvmValue, variable.llvmValue)

proc buildNot(backend: LLVMBackend, value: LLVMVar): LLVMVar =
    let cmp = llvm.buildICmp(backend.builder,
                             llvm.IntEQ,
                             value.llvmValue,
                             llvm.constInt(value.llvmValue.typeOfX(), 0, llvm.False),
                             "")
    newLLVMVar(llvm.buildZExt(backend.builder, cmp, value.llvmValue.typeOfX(), ""),
               Type(kind: Boolean))

proc build(backend: LLVMBackend, code: ByteCode, startIndex: int = 0, stopIndex: int = code.opcodes.len()) =
    template pop(): LLVMVar =
        backend.stack.pop()

    template push(v: LLVMVar) =
        backend.stack.push(v)

    var opcodeIndex = startIndex

    while opcodeIndex < stopIndex:
        let opcode = code.opcodes[opcodeIndex]

        template buildJump(condition: LLVMVar) =
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
            discard llvm.buildCondBr(backend.builder, condition.llvmValue, thenBlock, jumpBlock)

            llvm.positionBuilderAtEnd(backend.builder, jumpBlock)

        case opcode.kind:
        of StoreVar:
            let value = pop()

            var variable = backend.variables.getOrDefault(opcode.value, nil)
            if variable == nil:
                let createVar = if backend.isGlobalScope(): createGlobal else: createVariable
                backend.variables[opcode.value] = createVar(backend, opcode.value, value.typ)
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

            push(newLLVMVar(llvmValue, variableType))

        of Compare:
            let right = pop()
            let left = pop()

            proc buildCmp(signedPredicate: llvm.IntPredicate,
                          unsignedPredicate: llvm.IntPredicate,
                          realPredicate: llvm.RealPredicate): llvm.ValueRef =
                if left.isReal():
                    return llvm.buildFCmp(backend.builder, realPredicate, left.llvmValue, right.llvmValue, "")
                elif left.isSigned():
                    return llvm.buildICmp(backend.builder, signedPredicate, left.llvmValue, right.llvmValue, "")
                else:
                    return llvm.buildICmp(backend.builder, unsignedPredicate, left.llvmValue, right.llvmValue, "")

            let condition = case opcode.compare:
            of Equal:
                buildCmp(llvm.IntEQ, llvm.IntEQ, llvm.RealOEQ)
            of BiggerThan:
                buildCmp(llvm.IntSGT, llvm.IntUGT, llvm.RealOGT)
            of BiggerThanEqual:
                buildCmp(llvm.IntSGE, llvm.IntUGE, llvm.RealOGE)
            of SmallerThan:
                buildCmp(llvm.IntSLT, llvm.IntULT, llvm.RealOLT)
            of SmallerThanEqual:
                buildCmp(llvm.IntSLE, llvm.IntULE, llvm.RealOLE)

            push(newLLVMVar(condition, Type(kind: Boolean)))

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
            let llvmValue = f(backend.builder, left.llvmValue, right.llvmValue, "")
            push(newLLVMVar(llvmValue, left.typ))

        of Subtract:
            let right = pop()
            let left = pop()

            let f = if left.isReal(): llvm.buildFSub else: llvm.buildSub
            let llvmValue = f(backend.builder, left.llvmValue, right.llvmValue, "")
            push(newLLVMVar(llvmValue, left.typ))

        of Multiply:
            let right = pop()
            let left = pop()

            let f = if left.isReal(): llvm.buildFMul else: llvm.buildMul
            let llvmValue = f(backend.builder, left.llvmValue, right.llvmValue, "")
            push(newLLVMVar(llvmValue, left.typ))

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
            var oldVariables = initTable[int, LLVMVar]()
            for key in backend.variables.keys():
                oldVariables[key] = backend.variables[key]

            # Now that we have created the entry block allocate the function arguments
            for i, t in opcode.arguments:
                let index = i + backend.variables.len()
                let variable = backend.createVariable(index, t)
                discard llvm.buildStore(backend.builder, llvmFunc.getParam(cast[cuint](i)), variable.llvmValue)
                backend.variables[index] = variable

            backend.functionsRetTypes[name] = opcode.ret

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
                args.add(pop().llvmValue)

            if llvmFunc.countParams() > 1:
                args.reverse()

            push(newLLVMVar(llvm.buildCall(backend.builder, llvmFunc, args, ""),
                            backend.functionsRetTypes[name]))

        of Return:
            discard llvm.buildRet(backend.builder, pop().llvmValue)

        of Cast:
            let left = pop()
            let right = opcode.toType

            let toLlvmType = backend.getLLVMType(right)

            let leftKind = left.llvmValue.typeOfX().getTypeKind()
            let rightKind = toLlvmType.getTypeKind()

            let variable = case leftKind:
            of llvm.IntegerTypeKind:
                case rightKind:
                of llvm.PointerTypeKind:
                    llvm.buildIntToPtr(backend.builder, left.llvmValue, toLlvmType, "")
                else:
                    # TODO: integer to integer signed / unsigned casts
                    # reference: https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/basic-constructs/casts.html
                    llvm.buildBitCast(backend.builder, left.llvmValue, toLlvmType, "")

            of llvm.PointerTypeKind:
                case rightKind:
                of llvm.PointerTypeKind:
                    llvm.buildPointerCast(backend.builder, left.llvmValue, toLlvmType, "")
                of llvm.IntegerTypeKind:
                    llvm.buildPtrToInt(backend.builder, left.llvmValue, toLlvmType, "")
                else:
                    llvm.buildBitCast(backend.builder, left.llvmValue, toLlvmType, "")

            else:
                llvm.buildBitCast(backend.builder, left.llvmValue, toLlvmType, "")

            push(newLLVMVar(variable, right))

        else:
            raise newBackendError(fmt"unsupported opcode: {opcode.kind}")

        inc(opcodeIndex)

proc feed*(backend: LLVMBackend, code: ByteCode) =
    backend.build(code)
