type
    TokenKind* = enum
        Unknown
        NewLine
        Indentation

        Symbol
        Number

        If
        Else
        ElseIf
        Let
        Proc
        Ret
        Import
        Pass

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

        SmallArrow

    Token* = ref object
        case kind*: TokenKind
        of Symbol, Number:
            value*: string
        of Indentation:
            length*: int
        else:
            discard
