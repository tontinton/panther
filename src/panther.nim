import os
import tables
import options

import frontend
import backend
import ast
import bytecodebuilder
import opcodes

const TMP_ASM_FILE = "tmp_output.S"
const DEFAULT_INPUT_FILE = "main.pan"
const DEFAULT_OUTPUT_FILE = "output.o"
const DEFAULT_TARGET = "x86_64-unknown-linux-gnu"

proc printAst(input: string = DEFAULT_INPUT_FILE) =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        echo outputAst.get()

proc printAsm(input: string = DEFAULT_INPUT_FILE,
              target: string = DEFAULT_TARGET) =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        let code = outputAst.get().byteCode()
        if code.compile(TMP_ASM_FILE, target, outputAsm=true):
            try:
                echo readFile(TMP_ASM_FILE)
            finally:
                removeFile(TMP_ASM_FILE)

proc compileFile(input: string = DEFAULT_INPUT_FILE,
                 output: string = DEFAULT_OUTPUT_FILE,
                 target: string = DEFAULT_TARGET,
                 assembly: bool = false) =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        let code = outputAst.get().byteCode()
        discard code.compile(output, target, outputAsm=assembly)

proc printByteCode(input: string = DEFAULT_INPUT_FILE) =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        echo outputAst.get().byteCode()

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
    ], [
        printByteCode,
        doc="print the generated byte code of the input file given",
        cmdName="bytecode",
    ])
