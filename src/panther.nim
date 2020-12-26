import tables
import options

import frontend
import backend
import ast

proc printAst(input: string = "main.pan") =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        echo outputAst.get()

proc compileFile(input: string = "main.pan",
                 output: string = "output.o",
                 target: string = "x86_64-unknown-linux-gnu") =
    let outputAst = input.parseFile()
    if outputAst.isSome():
        let expression = outputAst.get()
        expression.compile(output, target)


when isMainModule:
    import cligen
    dispatchMulti([
        printAst,
        doc="print the generated ast of the input file given",
        cmdName="ast",
    ], [
        compileFile,
        doc="compile the input file given",
        cmdName="compile",
    ])