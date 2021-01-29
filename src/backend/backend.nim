import strformat

import llvm_backend
import common/customerrors
import frontend/ast

proc compile*(expression: Expression, outputPath: string, target: string, outputAsm: bool = false): bool =
    try:
        let backend = newLLVMBackend()
        backend.feed(expression)
        backend.compile(outputPath, target, outputAsm=outputAsm)
        true
    except LLVMError as e:
        echo &"\nLLVM Backend error: {e.msg}"
        false
