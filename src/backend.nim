import strformat

import ast
import llvm_backend

proc compile*(expression: Expression, outputPath: string = "output.o", target: string = "x86_64-unknown-linux-gnu") =
    try:
        let backend = newLLVMBackend()
        backend.feed(expression)
        backend.compile(outputPath, target)
    except LibraryError as e:
        echo &"\nBackend error: {e.msg}"
