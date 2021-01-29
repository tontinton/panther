import common/customerrors

type
    TokenKind* = enum
        Unknown
        NewLine
        Indentation

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

        Comma
        Colon
        ColonNewLine
        SemiColon
        OpenBracket
        CloseBracket
        Pound

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

        SmallArrow

    Token* = ref object
        errorInfo*: ErrorInfo

        case kind*: TokenKind
        of Symbol, Number, Float, Str, Unknown:
            value*: string
        of Indentation:
            indentation*: int
        else:
            discard

proc error*(token: Token): seq[ErrorInfo] =
    @[token.errorInfo]
