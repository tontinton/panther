import strutils
import strformat
import algorithm
import tables
import stacks

import llvm/llvm

import common/[types, customerrors]
import ir/[opcodes, variables]

const GLOBAL_LEVEL = 0
const OUTPUT_SECTION = ".shell"

type
    LLVMVar = ref object
        llvmValue: llvm.ValueRef
        typ: types.Type

    LLVMJump = ref object
        preJumpBlock: llvm.BasicBlockRef
        jumpBlock: llvm.BasicBlockRef
        condition: LLVMVar

    LLVMBackend* = ref object
        context: ContextRef
        module: ModuleRef
        builder: BuilderRef
        types: TableRef[types.TypeKind, llvm.TypeRef]
        variables: TableRef[int, LLVMVar]
        stack: Stack[LLVMVar]
        level: int

        # For each jump opcode, we create a basic block and put it inside this table.
        # Once, we reach an opcode index that resides inside this table,
        # we go back to the basic block and append a jump instruction.
        jumps: TableRef[int, seq[LLVMJump]]

proc newLLVMVar(v: llvm.ValueRef, t: types.Type): LLVMVar =
    LLVMVar(llvmValue: v, typ: t)

proc newLLVMJump(preJump: llvm.BasicBlockRef,
                 jump: llvm.BasicBlockRef,
                 condition: LLVMVar = nil): LLVMJump =
    LLVMJump(preJumpBlock: preJump, jumpBlock: jump, condition: condition)

proc newLLVMBackend*(): LLVMBackend =
    let context = llvm.getGlobalContext()
    let module = llvm.moduleCreateWithNameInContext("module", context)
    let builder = llvm.createBuilderInContext(context)

    let types = newTable[types.TypeKind, llvm.TypeRef]()
    let voidType = llvm.voidTypeInContext(context)
    let int8Type = llvm.intTypeInContext(context, 8)
    let int16Type = llvm.intTypeInContext(context, 16)
    let int32Type = llvm.intTypeInContext(context, 32)
    let int64Type = llvm.intTypeInContext(context, 64)

    let halfType = llvm.halfTypeInContext(context)
    let floatType = llvm.floatTypeInContext(context)
    let doubleType = llvm.doubleTypeInContext(context)

    types[Void] = voidType

    types[Boolean] = int8Type
    types[String] = llvm.pointerType(int8Type)

    types[Signed8] = int8Type
    types[Signed16] = int16Type
    types[Signed32] = int32Type
    types[Signed64] = int64Type

    types[Unsigned8] = int8Type
    types[Unsigned16] = int16Type
    types[Unsigned32] = int32Type
    types[Unsigned64] = int64Type

    types[Float16] = halfType
    types[Float32] = floatType
    types[Float64] = doubleType

    LLVMBackend(context: context,
                module: module,
                builder: builder,
                types: types,
                variables: newTable[int, LLVMVar](),
                stack: Stack[LLVMVar](),
                level: GLOBAL_LEVEL,
                jumps: newTable[int, seq[LLVMJump]]())

proc optimize(backend: LLVMBackend) =
    # TODO: align-all-blocks=1 / align-all-functions=1
    let pmb = llvm.passManagerBuilderCreate()
    pmb.passManagerBuilderSetOptLevel(2)
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
    case typ.kind:
    of Procedure:
        let retType = backend.getLLVMType(typ.ret)
        var paramTypes: seq[llvm.TypeRef] = @[]

        for (_, t) in typ.params:
            paramTypes.add(backend.getLLVMType(t))

        result = llvm.pointerType(llvm.functionType(retType, paramTypes))
    else:
        result = backend.types[typ.kind]
        for i in 0..<typ.ptrLevel:
            result = llvm.pointerType(result)

proc isGlobalScope(backend: LLVMBackend): bool =
    backend.level == GLOBAL_LEVEL

proc createLocalVariable(backend: LLVMBackend, index: int, typ: Type): LLVMVar =
    let pre = backend.builder.getInsertBlock()
    let llvmFunc = pre.getBasicBlockParent()
    let entryBlock = llvmFunc.getEntryBasicBlock()

    llvm.positionBuilderAtEnd(backend.builder, entryBlock)
    let variable = llvm.buildAlloca(backend.builder, backend.getLLVMType(typ), intToStr(index))
    llvm.positionBuilderAtEnd(backend.builder, pre)

    newLLVMVar(variable, typ)

proc createGlobal(backend: LLVMBackend, index: int, typ: Type): LLVMVar =
    let global = llvm.addGlobal(backend.module, backend.getLLVMType(typ), intToStr(index))
    llvm.setGlobalConstant(global, llvm.False)
    llvm.setSection(global, OUTPUT_SECTION)
    newLLVMVar(global, typ)

proc createVariable(backend: LLVMBackend, index: int, typ: Type): LLVMVar =
    let createVar = if backend.isGlobalScope(): createGlobal else: createLocalVariable
    createVar(backend, index, typ)

proc isReal(val: LLVMVar): bool =
    return val.typ.isRealInteger()

proc isSigned(val: LLVMVar): bool =
    return val.typ.isSignedInteger()

proc isUnsigned(val: LLVMVar): bool =
    return val.typ.isUnsignedInteger()

proc load(backend: LLVMBackend, variable: LLVMVar): LLVMVar =
    let v = if backend.isGlobalScope():
        variable.llvmValue.getInitializer()
    else:
        llvm.buildLoad(backend.builder, variable.llvmValue, "")
    newLLVMVar(v, variable.typ)

proc store(backend: LLVMBackend, variable: LLVMVar, value: LLVMVar) =
    if backend.isGlobalScope():
        llvm.setInitializer(variable.llvmValue, value.llvmValue)
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

proc buildJump(backend: LLVMBackend, jumpTo: int, condition: LLVMVar = nil) =
    let startBlock = backend.builder.getInsertBlock()
    let llvmFunc = startBlock.getBasicBlockParent()

    let preJumpBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "prejump")

    if llvm.getBasicBlockTerminator(startBlock) == nil:
        llvm.positionBuilderAtEnd(backend.builder, startBlock)
        discard llvm.buildBr(backend.builder, preJumpBlock)
    llvm.positionBuilderAtEnd(backend.builder, preJumpBlock)

    let jumpBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "jump")
    llvm.positionBuilderAtEnd(backend.builder, jumpBlock)

    if not backend.jumps.contains(jumpTo):
        backend.jumps[jumpTo] = @[]

    backend.jumps[jumpTo].add(newLLVMJump(preJumpBlock, jumpBlock, condition=condition))

proc build(backend: LLVMBackend, code: ByteCode) =
    template pop(): LLVMVar =
        backend.stack.pop()

    template push(v: LLVMVar) =
        backend.stack.push(v)

    var opcodeIndex = 0

    while opcodeIndex < code.opcodes.len():
        let opcode = code.opcodes[opcodeIndex]

        let blocks = backend.jumps.getOrDefault(opcodeIndex, @[])
        if blocks.len() > 0:
            # Create jumps to here
            let startBlock = backend.builder.getInsertBlock()
            let llvmFunc = startBlock.getBasicBlockParent()
            let hereBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "here")
            llvm.positionBuilderAtEnd(backend.builder, hereBlock)

            for b in blocks:
                llvm.positionBuilderAtEnd(backend.builder, b.preJumpBlock)

                if b.condition != nil:
                    discard llvm.buildCondBr(backend.builder, b.condition.llvmValue, b.jumpBlock, hereBlock)
                else:
                    discard llvm.buildBr(backend.builder, hereBlock)

                if llvm.getBasicBlockTerminator(b.jumpBlock) == nil:
                    llvm.positionBuilderAtEnd(backend.builder, b.jumpBlock)
                    discard llvm.buildBr(backend.builder, hereBlock)

            llvm.positionBuilderAtEnd(backend.builder, hereBlock)

            # Clear handled jumps
            backend.jumps.del(opcodeIndex)

        case opcode.kind:
        of StoreVar:
            let value = pop()

            var variable = backend.variables.getOrDefault(opcode.value, nil)
            if variable == nil:
                backend.variables[opcode.value] = backend.createVariable(opcode.value, value.typ)
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
            let llvmValue = if variableType.isSignedInteger():
                llvm.constInt(llvmType,
                              cast[culonglong](parseInt(variable.value)),
                              llvm.True)
            elif variableType.isUnsignedInteger():
                llvm.constInt(llvmType,
                              cast[culonglong](parseInt(variable.value)),
                              llvm.False)
            elif variableType.isRealInteger():
                llvm.constRealOfString(llvmType, variable.value)
            else:
                case variableType.kind:
                of String:
                    llvm.constString(variable.value, variable.value.len().cuint, llvm.False)
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
                if left.isUnsigned():
                    return llvm.buildICmp(backend.builder, unsignedPredicate, left.llvmValue, right.llvmValue, "")
                elif left.isReal():
                    return llvm.buildFCmp(backend.builder, realPredicate, left.llvmValue, right.llvmValue, "")
                elif left.isSigned():
                    return llvm.buildICmp(backend.builder, signedPredicate, left.llvmValue, right.llvmValue, "")
                else:
                    raise newBackendError(fmt"unsupported compare: {left.typ.kind}")

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
            backend.buildJump(opcode.value, condition=pop())

        of JumpTrue:
            backend.buildJump(opcode.value, condition=backend.buildNot(pop()))

        of opcodes.Jump:
            backend.buildJump(opcode.value)

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

            var llvmFunc = llvm.getNamedFunction(backend.module, name)
            if llvmFunc == nil:
                # Create function in llvm
                let ourRetType = opcode.funcType.ret
                let llvmRetType = backend.getLLVMType(ourRetType)
                var paramTypes: seq[llvm.TypeRef] = @[]

                for (_, t) in opcode.functype.params:
                    paramTypes.add(backend.getLLVMType(t))

                let funcType = llvm.functionType(llvmRetType, paramTypes)
                llvmFunc = llvm.addFunction(backend.module, name, funcType)

                llvm.setSection(llvmFunc, OUTPUT_SECTION)
                llvm.setFunctionCallConv(llvmFunc, llvm.CCallConv.cuint)

                for i, (_, t) in opcode.funcType.params:
                    let index = i + backend.variables.len()
                    llvmFunc.getParam(cast[cuint](i)).setValueName(intToStr(index))

                backend.variables[opcode.index] = newLLVMVar(llvmFunc, opcode.funcType)

            if opcode.code.opcodes.len() > 0:
                let entryBlock = llvm.appendBasicBlockInContext(backend.context, llvmFunc, "entry")
                llvm.positionBuilderAtEnd(backend.builder, entryBlock)

                # TODO: better copy
                var oldVariables = initTable[int, LLVMVar]()
                for key in backend.variables.keys():
                    oldVariables[key] = backend.variables[key]

                # Now that we have created the entry block allocate the function arguments
                for i, (_, t) in opcode.functype.params:
                    let index = i + backend.variables.len()
                    let variable = backend.createLocalVariable(index, t)
                    discard llvm.buildStore(backend.builder, llvmFunc.getParam(cast[cuint](i)), variable.llvmValue)
                    backend.variables[index] = variable

                inc(backend.level)
                backend.build(opcode.code)  # TODO: LLVMVerifyFunction
                dec(backend.level)

                backend.variables.clear()
                for key in oldVariables.keys():
                    backend.variables[key] = oldVariables[key]

        of opcodes.Call:
            let variable = pop()
            let llvmFunc = variable.llvmValue

            var args: seq[llvm.ValueRef] = @[]
            let argsLen = variable.typ.params.len()

            for _ in 0..<argsLen:
                args.add(pop().llvmValue)

            if argsLen > 1:
                args.reverse()

            push(newLLVMVar(llvm.buildCall(backend.builder, llvmFunc, args, ""), variable.typ.ret))

        of Return:
            let val = pop()
            if llvm.getBasicBlockTerminator(backend.builder.getInsertBlock()) == nil:
                discard llvm.buildRet(backend.builder, val.llvmValue)

        of ReturnNothing:
            discard llvm.buildRetVoid(backend.builder)

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
