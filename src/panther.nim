import tables

import clapfn

import lexer
import parser
import ast
import analyzer

var argParser = ArgumentParser(programName: "panther",
                               fullName: "Panther lang compiler",
                               description: "A modern shellcode compiler.",
                               version: "0.1.0")
argParser.addRequiredArgument("input", "Input file.")
let args = argParser.parse()
let input = args["input"]

let inputLexer = newLexer(readFile(input))
let topLevelParser = newParser()

let inputAst = topLevelParser.parseBlock(inputLexer.tokens())
inputAst.analyze()
echo inputAst
