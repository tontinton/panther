import sequtils
import strutils
import strformat
import options

import tokens
import frontenderrors

type
    Lexer* = ref object
        text: string
        path*: Option[string]

func newLexer*(text: string): Lexer =
    Lexer(text: text.replace("\r\n", "\n"), path: none[string]())

proc newLexerFromFile*(path: string): Lexer =
    Lexer(text: path.readFile().replace("\r\n", "\n"), path: some(path))

func getLine*(lexer: Lexer, lineStartIndex: int): string =
    var index = lineStartIndex
    while index < lexer.text.len() and lexer.text[index] != '\n':
        inc(index)
    lexer.text[lineStartIndex..index - 1]

func getString(line: string, index: int): string =
    var currentIndex = index
    while currentIndex < line.len() and line[currentIndex] != '"':
        inc(currentIndex)
    line[index..<currentIndex]

func getNumber(line: string, index: int): string =
    var currentIndex = index
    while currentIndex < line.len():
        case line[currentIndex]:
        of '0'..'9', '.':
            inc(currentIndex)
        else:
            break
    line[index..<currentIndex]

func getSymbol(line: string, index: int): string =
    var currentIndex = index
    while currentIndex < line.len():
        case line[currentIndex]:
        of 'A'..'Z', 'a'..'z', '0'..'9', '_':
            inc(currentIndex)
        else:
            break
    line[index..<currentIndex]

iterator items*(lexer: Lexer): Token =
    var indentationLength = 0
    var inIndentation = true
    var index = 0
    var line = 1
    var lineStart = 0

    proc newToken(kind: TokenKind, length: int = 1): Token = 
        Token(kind: kind, errorInfo: newErrorInfo(line, lineStart, index - lineStart, length))

    proc newUnknown(value: string): Token =
        Token(kind: Unknown, value: value, errorInfo: newErrorInfo(line, lineStart, index - lineStart, value.len()))

    proc newSymbol(value: string): Token =
        Token(kind: Symbol, value: value, errorInfo: newErrorInfo(line, lineStart, index - lineStart, value.len()))

    proc newNumber(value: string): Token =
        Token(kind: Number, value: value, errorInfo: newErrorInfo(line, lineStart, index - lineStart, value.len()))

    proc newStr(value: string): Token =
        Token(kind: Str, value: value, errorInfo: newErrorInfo(line, lineStart, index - lineStart - 1, value.len() + 2))

    proc newIndentation(): Token =
        Token(kind: Indentation,
              indentation: indentationLength,
              errorInfo: newErrorInfo(line, lineStart, 0, index - lineStart))

    while index < lexer.text.len():
        let c = lexer.text[index]
        if inIndentation:
            case c:
            of ' ':
                indentationLength += 1
            of '\t':
                indentationLength += 4
            else:
                inIndentation = false
                yield newIndentation()
                continue
        else:
            case c:
            of ' ', '\t':
                discard
            of '\n':
                yield newToken(NewLine)
                inIndentation = true
                indentationLength = 0
                inc(line)
                lineStart = index + 1
            of ',':
                yield newToken(Comma)
            of ':':
                if index + 1 < lexer.text.len() and lexer.text[index + 1] == '\n':
                    yield newToken(ColonNewLine)
                    inc(index)
                    inIndentation = true
                    indentationLength = 0
                    inc(line)
                    lineStart = index + 1
                else:
                    yield newToken(Colon)
            of ';':
                yield newToken(SemiColon)
            of '(':
                yield newToken(OpenBracket)
            of ')':
                yield newToken(CloseBracket)
            of '#':
                yield newToken(Pound)
            of '-':
                if index + 1 < lexer.text.len():
                    case lexer.text[index + 1]:
                    of '>':
                        yield newToken(SmallArrow, 2)
                        inc(index)
                    of '=':
                        yield newToken(MinusEqual, 2)
                        inc(index)
                    of '0'..'9', '.':
                        let value = getNumber(lexer.text, index + 1)
                        yield newNumber(fmt"-{value}")
                        index += value.len()
                    else:
                        yield newToken(Minus)
                else:
                    yield newToken(Minus)
            of '+':
                if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    yield newToken(PlusEqual, 2)
                    inc(index)
                else:
                    yield newToken(Plus)
            of '/':
                if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    yield newToken(DivEqual, 2)
                    inc(index)
                else:
                    yield newToken(Div)
            of '*':
                if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    yield newToken(MulEqual, 2)
                    inc(index)
                else:
                    yield newToken(Mul)
            of '=':
                if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    yield newToken(DoubleEqual, 2)
                    inc(index)
                else:
                    yield newToken(Equal)
            of '>':
                if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    yield newToken(BiggerThanEqual, 2)
                    inc(index)
                else:
                    yield newToken(BiggerThan)
            of '<':
                if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    yield newToken(SmallerThanEqual, 2)
                    inc(index)
                else:
                    yield newToken(SmallerThan)
            of '"':
                inc(index)
                let value = getString(lexer.text, index)
                yield newStr(value)
                index += value.len()
            of '0'..'9', '.':
                let value = getNumber(lexer.text, index)
                yield newNumber(value)
                index += value.len() - 1
            of 'A'..'Z', 'a'..'z':
                let value = getSymbol(lexer.text, index)
                index += value.len() - 1
                case value:
                of "if": yield newToken(If)
                of "else": yield newToken(Else)
                of "elif": yield newToken(ElseIf)
                of "let": yield newToken(Let)
                of "proc": yield newToken(Proc)
                of "return": yield newToken(Ret)
                of "import": yield newToken(Import)
                of "pass": yield newToken(Pass)
                of "true": yield newToken(True)
                of "false": yield newToken(False)
                of "and": yield newToken(And)
                of "or": yield newToken(Or)
                of "not": yield newToken(Not)
                else: yield newSymbol(value)
            else:
                yield newUnknown($c)
        inc(index)

func tokens*(lexer: Lexer): seq[Token] {.noSideEffect.} =
    result = toSeq(lexer)
