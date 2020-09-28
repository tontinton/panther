import tables

import clapfn

import lexer
import parser
import ast

var argParser = ArgumentParser(programName: "panther",
                               fullName: "Panther lang compiler",
                               description: "A modern shellcode compiler.",
                               version: "0.1.0")
argParser.addRequiredArgument("input", "Input file.")
let args = argParser.parse()
let input = args["input"]
let l = newLexer(readFile(input))
let p = newParser()
echo p.parseBlock(l.tokens())
