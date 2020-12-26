import strformat

import ast
import llvm_backend

proc compile*(expression: Expression, outputPath: string, target: string) =
    try:
        let backend = newLLVMBackend()
        backend.feed(expression)
        backend.compile(outputPath, target)
    except LibraryError as e:
        echo &"\nBackend error: {e.msg}"
