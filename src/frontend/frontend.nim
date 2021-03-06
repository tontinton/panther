import std/exitprocs
import options
import strformat
import terminal
import strutils

import optionsutils
import stacks

import lexer
import parser
import ast
import analyzer
import common/customerrors

proc printErrorLine(message: string) =
    setForegroundColor(fgRed)
    stdout.write("error: ")
    setForegroundColor(fgWhite)
    echo message

proc printSingle(e: ParseError, lexer: Lexer) =
    addExitProc(resetAttributes)

    e.msg.printErrorLine()

    for info in e.info:
        let linePrefix = fmt"{info.line} | "

        setForegroundColor(fgCyan)
        stdout.write(" ".repeat(linePrefix.len() - 3) & "--> ")
        setForegroundColor(fgWhite)

        withSome lexer.path:
            some path:
                echo fmt"{path}:{info.line}:{info.start}"
            none:
                echo fmt"TEXT_INPUT:{info.line}:{info.start}"

        setForegroundColor(fgCyan)
        stdout.write(linePrefix)

        setForegroundColor(fgWhite)
        echo lexer.getLine(info.lineStart)

        setForegroundColor(fgRed)
        stdout.write(" ".repeat(linePrefix.len() + info.start))
        echo "^".repeat(info.length)

    resetAttributes()

proc print(e: ParseError, lexer: Lexer) =
    var errors = Stack[ParseError]()
    var error = e
    while not error.isNil():
        errors.push(error)
        error = error.next

    echo ""
    let s = if errors.len() > 1: "s" else: ""
    (&"got {errors.len()} compilation error{s}\n").printErrorLine()

    while not errors.isEmpty():
        errors.pop().printSingle(lexer)

proc parse(inputLexer: Lexer): Option[Expression] =
    let topLevelParser = newParser()
    try:
        let outputAst = topLevelParser.parseBlock(inputLexer.tokens())
        outputAst.analyze()
        return some(outputAst)
    except ParseError as e:
        e.print(inputLexer)
        return none[Expression]()

proc parseText*(input: string): Option[Expression] =
    let lexer = newLexer(input)
    try:
        lexer.parse()
    finally:
        lexer.close()

proc parseFile*(path: string): Option[Expression] =
    let lexer = newLexerFromFile(path)
    try:
        lexer.parse()
    finally:
        lexer.close()
