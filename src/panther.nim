import os
import tables
import options

import frontend
import backend
import ast

const TMP_ASM_FILE = "tmp_output.S"

proc printAst(input: string = "main.pan") =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        echo outputAst.get()

proc printAsm(input: string = "main.pan",
              target: string = "x86_64-unknown-linux-gnu") =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        let expression = outputAst.get()
        expression.compile(TMP_ASM_FILE, target, outputAsm=true)
        try:
            echo readFile(TMP_ASM_FILE)
        finally:
            removeFile(TMP_ASM_FILE)

proc compileFile(input: string = "main.pan",
                 output: string = "output.o",
                 target: string = "x86_64-unknown-linux-gnu",
                 assembly: bool = false) =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        let expression = outputAst.get()
        expression.compile(output, target, outputAsm=assembly)

when isMainModule:
    import cligen
    dispatchMulti([
        printAst,
        doc="print the generated ast of the input file given",
        cmdName="ast",
    ], [
        printAsm,
        doc="print the compiled assembly of the input file given",
        cmdName="asm",
    ], [
        compileFile,
        doc="compile the input file given",
        cmdName="compile",
        help={
            "assembly": "compile to asm"
        }
    ])