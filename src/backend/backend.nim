import strformat

import llvm_backend
import common/customerrors
import ir/opcodes

proc compile*(code: ByteCode, outputPath: string, target: string, outputAsm: bool = false): bool =
    try:
        let backend = newLLVMBackend()
        backend.feed(code)
        backend.compile(outputPath, target, outputAsm=outputAsm)
        true
    except LLVMError as e:
        echo &"\nLLVM Backend error: {e.msg}"
        false
