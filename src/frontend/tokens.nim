import common/customerrors

type
    TokenKind* = enum
        Unknown
        NewLine
        DoubleSlash

        Symbol
        Number
        Float
        Str

        If
        Else
        ElseIf
        Let
        Proc
        Ret
        Import
        Pass
        True
        False

        While
        Break
        Continue

        Comma
        Colon
        SemiColon
        OpenBracket
        CloseBracket
        OpenCurly
        CloseCurly

        Minus
        MinusEqual
        Plus
        PlusEqual
        Div
        DivEqual
        Mul
        MulEqual
        Equal
        DoubleEqual
        BiggerThan
        BiggerThanEqual
        SmallerThan
        SmallerThanEqual
        And
        Or
        Not
        Ampersand
        As
        Extern

        SmallArrow

    Token* = ref object
        errorInfo*: ErrorInfo

        case kind*: TokenKind
        of Symbol, Number, Float, Str, Unknown:
            value*: string
        else:
            discard

proc error*(token: Token): seq[ErrorInfo] =
    @[token.errorInfo]
