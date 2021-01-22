import options
import strformat

import safeoptions

import ast
import tokens
import types
import arithmetic
import customerrors

type
    Parser* = ref object
        indentation: int
        separator: TokenKind
        stopper: TokenKind
        error: ParseError

        arithmetic: bool  # Are we currently parsing an arithmetic expression

        # When building rolling expressions,
        # you can use this variable to not continue the nextExpression recusrion,
        # instead returning immediately after a single expression.
        # Example: Ref / Deref are unary operations that only care about the first expression after them,
        # which is opposite from the Not expression (not 1 + 1) should be parsed differently than (*1 + 1).
        #
        # The reason for keeping the level and not just a boolean is because of recursion,
        # Example: **ptr / &*&*&*&*variable
        breakExpressionLevel: int

    ParserState = ref object
        tokens: seq[Token]
        index: int
        emptyExpression: Expression
        separator: Option[TokenKind]

func newParserState(tokens: seq[Token]): ParserState =
    ParserState(tokens: tokens,
                index: 0,
                emptyExpression: Expression(kind: Empty),
                separator: none[TokenKind]())

func newParser*(indentation: int = 0,
                separator: TokenKind = NewLine,
                stopper: TokenKind = Indentation): Parser =
    Parser(indentation: indentation,
           separator: separator,
           stopper: stopper,
           error: nil,
           arithmetic: false,
           breakExpressionLevel: 0)

proc addError(parser: Parser, e: ParseError) =
    if not parser.error.isNil():
        e.next = parser.error
    parser.error = e

proc nextExpression(parser: Parser,
                    state: ParserState,
                    prev: Expression = state.emptyExpression,
                    breakExpression: bool = false): Option[Expression]

proc nextExpressionOrError(parser: Parser, state: ParserState): Option[Expression] =
    try:
        return parser.nextExpression(state)
    except ParseError as e:
        while state.index < state.tokens.len():
            let token = state.tokens[state.index]
            if token.kind == parser.separator or (token.kind == parser.stopper and parser.stopper != Indentation):
                break
            inc(state.index)
        raise e

iterator parse(parser: Parser, state: ParserState): Expression =
    var expression : Option[Expression]

    var errorRaised = false
    var keepParsing = true

    try:
        expression = parser.nextExpressionOrError(state)
        if expression.isNone():
            keepParsing = false
    except ParseError as e:
        parser.addError(e)
        errorRaised = true

    while keepParsing:
        if not errorRaised and expression.isSome():
            yield expression.get()

        try:
            expression = parser.nextExpressionOrError(state)
            if expression.isNone():
                keepParsing = false
        except ParseError as e:
            parser.addError(e)
            errorRaised = true

    if not parser.error.isNil():
        raise parser.error

iterator parse*(parser: Parser, tokens: seq[Token]): Expression =
    var state = newParserState(tokens)
    for expression in parser.parse(state):
        yield expression

proc parseBlock*(parser: Parser, tokens: seq[Token]): Expression =
    result = Expression(kind: Block, expressions: @[])
    for expression in parser.parse(tokens):
        result.expressions.add(expression)

proc nextBlock(parser: Parser, state: ParserState): Expression =
    let blockParser = newParser(indentation = parser.indentation + 4)
    result = Expression(kind: Block, expressions: @[])
    try:
        for expression in blockParser.parse(state):
            result.expressions.add(expression)
    except ParseError as e:
        parser.addError(e)

proc nextHead(parser: Parser, state: ParserState): Expression =
    let blockParser = newParser(separator = Comma, stopper = CloseBracket)
    result = Expression(kind: Block, expressions: @[])
    try:
        for expression in blockParser.parse(state):
            result.expressions.add(expression)
    except ParseError as e:
        parser.addError(e)

proc buildUndeterminedType(state: ParserState, typeToken: Token): Type =
    let startIndex = state.index
    while state.index < state.tokens.len() and state.tokens[state.index].kind == Mul:
        inc(state.index)
    Type(kind: Undetermined, value: typeToken.value, ptrLevel: state.index - startIndex)

proc buildVoidType(): Type =
    Type(kind: Void, ptrLevel: 0)

proc buildUnaryTree(parser: Parser, state: ParserState, token: Token): Option[Expression] =
    # Used for unary expressions like Not,
    # `not 1 + 1` should be parsed as `not (1 + 1)`
    let expression = parser.nextExpression(state)
    if expression.isNone() or expression.get().isEmpty():
        raise newParseError(token, fmt"`{token.kind}` must have an expression after it")
    return some(Expression(kind: Unary, unaryExpr: expression.get(), token: token))

proc buildUnary(parser: Parser,
                state: ParserState,
                token: Token): Option[Expression] =
    # Used for unary expressions like Deref,
    # `deref 1 + 1` should be parsed as `(deref 1) + 1`
    let expression = parser.nextExpression(state, breakExpression=true)
    if expression.isNone() or expression.get().isEmpty():
        raise newParseError(token, fmt"`{token.kind}` must have an expression after it")
    return parser.nextExpression(state, Expression(kind: Unary, unaryExpr: expression.get(), token: token))

proc buildBinOp(parser: Parser,
                state: ParserState,
                token: Token,
                prev: Expression): Option[Expression] =
    case prev.kind:
    of Ident, Literal, FunctionCall, BinOp, Unary:
        discard
    else:
        raise newParseError(token, fmt"left side of `{token.kind}` cannot be `{prev.kind}`")

    let isRoot = parser.arithmetic == false
    parser.arithmetic = true

    let expression = parser.nextExpression(state)
    if expression.isNone() or expression.get().isEmpty():
        raise newParseError(token, fmt"`{token.kind}` must have an expression after it")

    let subtree = expression.get()

    let tree = case subtree.kind:
    of Ident, Literal, FunctionCall, BinOp, Unary:
        Expression(kind: BinOp, left: prev, right: subtree, token: token)
    else:
        raise newParseError(token, 
                            fmt"right side of `{token.kind}` cannot be `{subtree.kind}`")

    if isRoot:
        parser.arithmetic = false
        return some(tree.fixedArithmeticTree())
    else:
        return some(tree)

proc nextExpression(parser: Parser,
                    state: ParserState,
                    prev: Expression = state.emptyExpression,
                    breakExpression: bool = false): Option[Expression] =
    if parser.breakExpressionLevel > 0 and not breakExpression:
        dec(parser.breakExpressionLevel)
        return some(prev)

    if breakExpression:
        inc(parser.breakExpressionLevel)

    if state.index >= state.tokens.len():
        return none[Expression]()

    let token = state.tokens[state.index]
    inc(state.index)

    withSome state.separator:
        some separator:
            if token.kind == separator:
                return some(prev)

    if token.kind == parser.separator:
        return some(prev)

    case token.kind:
    of Indentation:
        if parser.stopper != Indentation or token.indentation == parser.indentation:
            return parser.nextExpression(state, prev)
        elif token.indentation < parser.indentation:
            dec(state.index)
            return none[Expression]()
        else:
            raise newParseError(token, "indentation of line cannot be larger without starting a new block")

    of Pound:
        inc(state.index)
        while state.index < state.tokens.len():
            case state.tokens[state.index].kind:
            of NewLine, ColonNewLine:
                inc(state.index)
                return some(prev)
            else:
                discard
            inc(state.index)
        return none[Expression]()

    of Let:
        let expression = parser.nextExpression(state)
        if expression.isNone():
            raise newParseError(token, "expected a symbol after `let`")
        return some(Expression(kind: Declaration,
                               declExpr: expression.get(),
                               token: token))

    of Colon:
        if prev.kind != Ident:
            raise newParseError(token, "expected `:` only after a symbol")
        else:
            if state.index < state.tokens.len() and state.tokens[state.index].kind == Symbol:
                let typeToken = state.tokens[state.index]
                inc(state.index)

                let exp = Expression(kind: TypedIdent,
                                     identType: buildUndeterminedType(state, typeToken),
                                     ident: prev,
                                     token: token)
                return parser.nextExpression(state, exp)
            else:
                raise newParseError(token, "expected a type after `:`")

    of SmallArrow:
        if prev.kind != FunctionCall:
            raise newParseError(token, "`->` can only come after a function declaration")
        return some(prev)

    of Proc:
        if state.index < state.tokens.len() and state.tokens[state.index].kind == Symbol:
            let expression = parser.nextExpression(state)
            if expression.isNone():
                raise newParseError(token, &"got an empty expression after {Proc}")
            let head = expression.get()
            if head.kind != FunctionCall:
                raise newParseError(token,
                                    &"expected a {FunctionCall} expression after proc's name, not {head.kind}")

            let returnType = if state.index < state.tokens.len() and state.tokens[state.index].kind == Symbol:
                let typeToken = state.tokens[state.index]
                inc(state.index)
                buildUndeterminedType(state, typeToken)
            else:
                buildVoidType()

            withSome parser.nextExpression(state):
                some procBlock:
                    return some(Expression(kind: FunctionDeclaration,
                                            declName: head.name,
                                            declParams: head.params,
                                            returnType: returnType,
                                            implementation: procBlock,
                                            token: token))
                none:
                    raise newParseError(token, fmt"expected an implementation for {head.name}")
        else:
            raise newParseError(token, "expected a symbol after `proc`")

    of Ret:
        let maybeExpression = parser.nextExpression(state)
        let expression = if maybeExpression.isNone():
            state.emptyExpression
        else:
            maybeExpression.get()
        return some(Expression(kind: Return, retExpr: expression, token: token))

    of OpenBracket:
        if prev.kind == Ident:
            let call = Expression(kind: FunctionCall,
                                  name: prev.value,
                                  params: parser.nextHead(state),
                                  token: token)
            return parser.nextExpression(state, call)
        else:
            let next = parser.nextHead(state)
            if next.expressions.len() == 1:
                return parser.nextExpression(state, next.expressions[0])
            else:
                return parser.nextExpression(state, next)

    of CloseBracket:
        if parser.stopper == CloseBracket:
            if prev.isEmpty():
                return none[Expression]()
            else:
                dec(state.index)
                return some(prev)
        else:
            raise newParseError(token, "unexpected `)`")

    of Symbol:
        return parser.nextExpression(state, Expression(kind: Ident, value: token.value, token: token))

    of Number:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: Signed32),
                                                       literal: token.value,
                                                       token: token))

    of Float:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: Float32),
                                                       literal: token.value,
                                                       token: token))

    of Str:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: String),
                                                       literal: token.value,
                                                       token: token))

    of True:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: Boolean),
                                                       literal: BOOLEAN_TRUE,
                                                       token: token))

    of False:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: Boolean),
                                                       literal: BOOLEAN_FALSE,
                                                       token: token))

    of ColonNewLine:
        if prev.isEmpty():
            let expression = parser.nextBlock(state)
            if expression.expressions.len() < 1:
                raise newParseError(token, "no block after `:`")
            return some(expression)
        else:
            dec(state.index)
            return some(prev)

    of If, ElseIf:
        if token.kind == ElseIf and prev.kind != IfThen:
            raise newParseError(token, "`elif` can only come after `if`")

        state.separator = some(ColonNewLine)
        var expression = parser.nextExpression(state)
        state.separator = none[TokenKind]()

        if expression.isNone():
            raise newParseError(token, fmt"no expression after `{token.kind}`")

        let condition = expression.get()

        dec(state.index)
        expression = parser.nextExpression(state)
        if expression.isNone():
            raise newParseError(token, fmt"no block after `{token.kind}`")

        let then = expression.get()
        let ifExpr = Expression(kind: IfThen, condition: condition, then: then, token: token)

        if state.index + 1 < state.tokens.len() and state.tokens[state.index].kind == Indentation:
            let nextToken = state.tokens[state.index + 1]
            case nextToken.kind:
            of ElseIf:
                expression = parser.nextExpression(state, ifExpr)
                if expression.isNone():
                    raise newParseError(token, fmt"no block after `{nextToken.kind}`")

                let elseExpr = expression.get()
                return some(Expression(kind: IfElseThen, ifThen: ifExpr, otherwise: elseExpr, token: token))
            of Else:
                return parser.nextExpression(state, ifExpr)
            else:
                return some(ifExpr)
        else:
            return some(ifExpr)

    of Else:
        if prev.kind != IfThen:
            raise newParseError(token, "`else` can only come after `if`")

        let expression = parser.nextExpression(state)
        if expression.isNone():
            raise newParseError(token, "no expression after `else`")

        let elseExpr = expression.get()
        if elseExpr.kind != Block:
            raise newParseError(token, fmt"expected a block, not `{elseExpr.kind}`")

        return some(Expression(kind: IfElseThen, ifThen: prev, otherwise: elseExpr, token: token))

    of Equal:
        case prev.kind:
        of Ident, TypedIdent, Unary:
            discard
        else:
            raise newParseError(token, "left side of `=` must be a valid assignment expression")

        let expression = parser.nextExpression(state)
        if expression.isNone() or expression.get().isEmpty():
            raise newParseError(token, "`=` must have an expression after it")

        let subtree = expression.get()
        case subtree.kind:
        of Ident, Literal, FunctionCall, BinOp, Unary:
            return some(Expression(kind: Assign, assignee: prev, assignExpr: subtree, token: token))
        else:
            raise newParseError(token, "right side of `=` must be a valid arithmetic expression")

    of Not:
        if not prev.isEmpty():
            raise newParseError(token, fmt"`{token.kind}` must be at the beginning of an expression")
        return parser.buildUnaryTree(state, token)

    of Ampersand:
        if not prev.isEmpty():
            raise newParseError(token, fmt"`{token.kind}` must be at the beginning of an expression")
        return parser.buildUnary(state, token)

    of Div, Plus, Minus, BiggerThan, BiggerThanEqual, SmallerThan, SmallerThanEqual, DoubleEqual, And, Or:
        if prev.isEmpty():
            raise newParseError(token, fmt"`{token.kind}` cannot be at the beginning of an expression")

        return parser.buildBinOp(state, token, prev)

    of Mul:
        if prev.isEmpty():
            return parser.buildUnary(state, token)
        else:
            return parser.buildBinOp(state, token, prev)

    of Unknown:
        raise newParseError(token, fmt"unexpected token: `{token.value}`")

    else:
        raise newParseError(token, fmt"unexpected token: `{token.kind}`")
