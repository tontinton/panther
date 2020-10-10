type
    TokenKind* = enum
        Unknown
        NewLine
        Indentation

        Symbol
        Number
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

        SmallArrow

    Token* = ref object
        line*: int
        lineStart*: int
        start*: int
        length*: int

        case kind*: TokenKind
        of Symbol, Number, Str:
            value*: string
        of Indentation:
            indentation*: int
        else:
            discard
