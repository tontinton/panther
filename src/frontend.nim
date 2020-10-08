import lexer
import parser
import ast
import analyzer

proc parse*(input: string): Expression =
    let inputLexer = newLexer(input)
    let topLevelParser = newParser()
    let outputAst = topLevelParser.parseBlock(inputLexer.tokens())
    outputAst.analyze()
    return outputAst
