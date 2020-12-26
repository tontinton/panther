import tables
import options

import clapfn

import frontend
import backend
import ast

var argParser = ArgumentParser(programName: "panther",
                               fullName: "Panther lang compiler",
                               description: "A modern shellcode compiler.",
                               version: "0.1.0")
argParser.addRequiredArgument("input", "Input file.")
let args = argParser.parse()
let input = args["input"]

let outputAst = input.parseFile()
if outputAst.isSome():
    let expression = outputAst.get()
    echo expression
    expression.compile()
