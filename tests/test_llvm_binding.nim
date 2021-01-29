import unittest
import strutils
import os
import strformat

import llvm/llvm

# To let the compiled test find the .so files
{.passL: "-Wl,-rpath,\\$ORIGIN".}
{.passL: "-Wl,-rpath,\\$ORIGIN/..".}

suite "llvm_binding":
    test "sanity":
        let context = llvm.getGlobalContext()
        let module = llvm.moduleCreateWithNameInContext("module", context)
        let builder = llvm.createBuilderInContext(context)
        let int32Type = llvm.intTypeInContext(context, 32)

        let funcType = llvm.functionType(int32Type, [])
        let main = llvm.addFunction(module, "func", funcType)

        let entryBlock = appendBasicBlockInContext(context, main, "entry")
        positionBuilderAtEnd(builder, entryBlock)

        let a = llvm.constInt(int32Type, 555, llvm.False)
        let b = llvm.constInt(int32Type, 111, llvm.False)
        let add = llvm.buildAdd(builder, a, b, "add")
        discard buildRet(builder, add)

        let llvmCode = llvm.printModuleToString(module)
        echo &"llvm generated code:\n\n{llvmCode}"

        initializeAllAsmPrinters()
        initializeAllTargets()
        initializeAllTargetInfos()
        initializeAllTargetMCs()

        let target = "x86_64-unknown-linux-gnu"

        var tr: llvm.TargetRef
        discard getTargetFromTriple(target, tr.addr, nil)

        let targetMachine = createTargetMachine(tr, target, "", "", llvm.CodeGenLevelDefault, llvm.RelocPIC, llvm.CodeModelDefault)

        var err: cstring
        assert llvm.False == llvm.targetMachineEmitToFile(targetMachine, module, "output.S", llvm.AssemblyFile, cast[cstringArray](addr(err)))
        let output = readFile("output.S")
        echo &"output.S:\n{output}"
        removeFile("output.S")
        assert "movl\t$666, %eax" in output
