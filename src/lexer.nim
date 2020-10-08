import sequtils
import strutils

import tokens

type
    Lexer* = ref object
        text: string

func newLexer*(text: string): Lexer =
    Lexer(text: text.replace("\r\n", "\n"))

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

iterator items*(lexer: Lexer): Token {.noSideEffect.} =
    var indentationLength = 0
    var inIndentation = true
    var index = 0
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
                yield Token(kind: Indentation, length: indentationLength)
                continue
        else:
            case c:
            of ' ', '\t':
                discard
            of '\n':
                yield Token(kind: NewLine)
                inIndentation = true
                indentationLength = 0
            of ',':
                yield Token(kind: Comma)
            of ':':
                yield if index + 1 < lexer.text.len() and lexer.text[index + 1] == '\n':
                    inc(index)
                    Token(kind: ColonNewLine)
                else:
                    Token(kind: Colon)
            of ';':
                yield Token(kind: SemiColon)
            of '(':
                yield Token(kind: OpenBracket)
            of ')':
                yield Token(kind: CloseBracket)
            of '#':
                yield Token(kind: Pound)
            of '-':
                yield if index + 1 < lexer.text.len():
                    case lexer.text[index + 1]:
                    of '>':
                        inc(index)
                        Token(kind: SmallArrow)
                    of '=':
                        inc(index)
                        Token(kind: MinusEqual)
                    else:
                        Token(kind: Minus)
                else:
                    Token(kind: Minus)
            of '+':
                yield if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    inc(index)
                    Token(kind: PlusEqual)
                else:
                    Token(kind: Plus)
            of '/':
                yield if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    inc(index)
                    Token(kind: DivEqual)
                else:
                    Token(kind: Div)
            of '*':
                yield if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    inc(index)
                    Token(kind: MulEqual)
                else:
                    Token(kind: Mul)
            of '=':
                yield if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    inc(index)
                    Token(kind: DoubleEqual)
                else:
                    Token(kind: Equal)
            of '>':
                yield if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    inc(index)
                    Token(kind: BiggerThanEqual)
                else:
                    Token(kind: BiggerThan)
            of '<':
                yield if index + 1 < lexer.text.len() and lexer.text[index + 1] == '=':
                    inc(index)
                    Token(kind: SmallerThanEqual)
                else:
                    Token(kind: SmallerThan)
            of '0'..'9', '.':
                let value = getNumber(lexer.text, index)
                index += value.len() - 1
                yield Token(kind: Number, value: value)
            of 'A'..'Z', 'a'..'z':
                let value = getSymbol(lexer.text, index)
                index += value.len() - 1
                case value:
                of "if":
                    yield Token(kind: If)
                of "else":
                    yield Token(kind: Else)
                of "elif":
                    yield Token(kind: ElseIf)
                of "let":
                    yield Token(kind: Let)
                of "proc":
                    yield Token(kind: Proc)
                of "return":
                    yield Token(kind: Ret)
                of "import":
                    yield Token(kind: Import)
                of "pass":
                    yield Token(kind: Pass)
                else:
                    yield Token(kind: Symbol, value: value)
            else:
                yield Token(kind: Unknown)
        inc(index)

func tokens*(lexer: Lexer): seq[Token] {.noSideEffect.} =
    result = toSeq(lexer)
