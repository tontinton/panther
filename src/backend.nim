import strformat

import ast
import llvm_backend

proc compile*(expression: Expression, outputPath: string, target: string, outputAsm: bool = false) =
    try:
        let backend = newLLVMBackend()
        backend.feed(expression)
        backend.compile(outputPath, target, outputAsm=outputAsm)
    except LibraryError as e:
        echo &"\nBackend error: {e.msg}"
