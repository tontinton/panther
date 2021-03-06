import sequtils
import strutils
import strformat
import options
import streams

import tokens
import common/customerrors

type
    Lexer* = ref object
        stream: Stream
        path*: Option[string]

proc newLexer*(text: string): Lexer =
    Lexer(stream: newStringStream(text), path: none[string]())

proc newLexerFromFile*(path: string): Lexer =
    Lexer(stream: newFileStream(path, fmRead), path: some(path))

proc newLexerFromStdin*(): Lexer =
    Lexer(stream: newFileStream(stdin), path: none[string]())

proc close*(lexer: Lexer) =
    lexer.stream.close()

proc readChar(lexer: Lexer): char =
    lexer.stream.readChar()

proc peekChar(lexer: Lexer): char =
    lexer.stream.peekChar()

proc position(lexer: Lexer): int =
    lexer.stream.getPosition()

proc setPosition(lexer: Lexer, pos: int) =
    lexer.stream.setPosition(pos)

proc getLine*(lexer: Lexer, lineStartIndex: int): string =
    let position = lexer.position()
    lexer.setPosition(lineStartIndex)
    result = lexer.stream.readLine()
    lexer.setPosition(position)

proc readString(lexer: Lexer): string =
    result = ""
    var c = lexer.readChar()
    while c != '\0' and c != '"':
        if c == '\\':
            let next = lexer.readChar()
            case next:
            of '\\':
                discard
            of 'r':
                c = '\r'
            of 'n':
                c = '\n'
            of 't':
                c = '\t'
            of 'v':
                c = '\v'
            of 'a':
                c = '\a'
            else:
                raise newException(LibraryError, &"\\{next} is not a valid character")

        result &= c
        c = lexer.readChar()

proc readNumber(lexer: Lexer): string =
    result = ""
    var c = lexer.peekChar()
    while c != '\0':
        case c:
        of '0'..'9', '.':
            discard lexer.readChar()
            result &= c
            c = lexer.peekChar()
        else:
            break

proc readHex(lexer: Lexer): string =
    result = ""
    var c = lexer.peekChar()
    while c != '\0':
        case c:
        of '0'..'9', 'a'..'f', 'A'..'F':
            discard lexer.readChar()
            result &= c
            c = lexer.peekChar()
        else:
            break

proc readSymbol(lexer: Lexer): string =
    result = ""
    var c = lexer.peekChar()
    while c != '\0':
        case c:
        of 'A'..'Z', 'a'..'z', '0'..'9', '_':
            discard lexer.readChar()
            result &= c
            c = lexer.peekChar()
        else:
            break

iterator items*(lexer: Lexer): Token =
    var line = 1
    var lineStart = 0

    proc newToken(kind: TokenKind, length: int = 1): Token =
        let start = lexer.position() - lineStart - length
        Token(kind: kind, errorInfo: newErrorInfo(line, lineStart, start, length))

    proc newUnknown(value: string): Token =
        let start = lexer.position() - lineStart - value.len()
        Token(kind: Unknown,
              value: value,
              errorInfo: newErrorInfo(line, lineStart, start, value.len()))

    proc newSymbol(value: string): Token =
        let start = lexer.position() - lineStart - value.len()
        Token(kind: Symbol, value: value, errorInfo: newErrorInfo(line, lineStart, start, value.len()))

    proc newNumber(value: string): Token =
        let start = lexer.position() - lineStart - value.len()
        if "." in value:
            Token(kind: Float, value: value, errorInfo: newErrorInfo(line, lineStart, start, value.len()))
        else:
            Token(kind: Number, value: value, errorInfo: newErrorInfo(line, lineStart, start, value.len()))

    proc newStr(value: string): Token =
        let start = lexer.position() - lineStart - value.len()
        Token(kind: Str, value: value, errorInfo: newErrorInfo(line, lineStart, start, value.len() + 2))

    var c = lexer.readChar()
    while c != '\0':
        case c:
        of ' ', '\t', '\r':
            discard
        of '\n':
            yield newToken(NewLine)
            inc(line)
            lineStart = lexer.position()
        of ',':
            yield newToken(Comma)
        of ':':
            yield newToken(Colon)
        of ';':
            yield newToken(SemiColon)
        of '{':
            yield newToken(OpenCurly)
        of '}':
            yield newToken(CloseCurly)
        of '(':
            yield newToken(OpenBracket)
        of ')':
            yield newToken(CloseBracket)
        of '&':
            yield newToken(Ampersand)
        of '-':
            case lexer.peekChar():
            of '>':
                discard lexer.readChar()
                yield newToken(SmallArrow, 2)
            of '=':
                discard lexer.readChar()
                yield newToken(MinusEqual, 2)
            of '0'..'9', '.':
                let value = lexer.readNumber()
                yield newNumber(fmt"-{value}")
            else:
                yield newToken(Minus)
        of '+':
            if lexer.peekChar() == '=':
                discard lexer.readChar()
                yield newToken(PlusEqual, 2)
            else:
                yield newToken(Plus)
        of '/':
            let peek = lexer.peekChar()
            if peek == '=':
                discard lexer.readChar()
                yield newToken(DivEqual, length=2)
            elif peek == '/':
                discard lexer.readChar()
                yield newToken(DoubleSlash, 2)
            else:
                yield newToken(Div)
        of '*':
            if lexer.peekChar() == '=':
                discard lexer.readChar()
                yield newToken(MulEqual, length=2)
            else:
                yield newToken(Mul)
        of '=':
            if lexer.peekChar() == '=':
                discard lexer.readChar()
                yield newToken(DoubleEqual, length=2)
            else:
                yield newToken(Equal)
        of '>':
            if lexer.peekChar() == '=':
                discard lexer.readChar()
                yield newToken(BiggerThanEqual, length=2)
            else:
                yield newToken(BiggerThan)
        of '<':
            if lexer.peekChar() == '=':
                discard lexer.readChar()
                yield newToken(SmallerThanEqual, length=2)
            else:
                yield newToken(SmallerThan)
        of '"':
            let value = lexer.readString()
            yield newStr(value)
        of '0'..'9', '.':
            if c == '0' and lexer.peekChar() == 'x':
                # hex number
                discard lexer.readChar()
                let value = lexer.readHex()
                yield newNumber($fromHex[BiggestUInt](&"0x{value}"))
            else:
                # regular number
                lexer.setPosition(lexer.position() - 1)
                let value = lexer.readNumber()
                yield newNumber(value)
        of 'A'..'Z', 'a'..'z', '_':
            lexer.setPosition(lexer.position() - 1)
            let value = lexer.readSymbol()
            let length = value.len()
            case value:
            of "if": yield newToken(If, length=length)
            of "else": yield newToken(Else, length=length)
            of "elif": yield newToken(ElseIf, length=length)
            of "let": yield newToken(Let, length=length)
            of "proc": yield newToken(Proc, length=length)
            of "return": yield newToken(Ret, length=length)
            of "while": yield newToken(While, length=length)
            of "break": yield newToken(Break, length=length)
            of "continue": yield newToken(Continue, length=length)
            of "import": yield newToken(Import, length=length)
            of "pass": yield newToken(Pass, length=length)
            of "true": yield newToken(True, length=length)
            of "false": yield newToken(False, length=length)
            of "and": yield newToken(And, length=length)
            of "or": yield newToken(Or, length=length)
            of "not": yield newToken(Not, length=length)
            of "as": yield newToken(As, length=length)
            of "extern": yield newToken(Extern, length=length)
            else: yield newSymbol(value)
        else:
            yield newUnknown($c)
        c = lexer.readChar()

proc tokens*(lexer: Lexer): seq[Token] =
    result = toSeq(lexer)
