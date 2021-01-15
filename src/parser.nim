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

    ParserState = ref object
        tokens: seq[Token]
        index: int
        emptyExpression: Expression
        separator: Option[TokenKind]
        arithmetic: bool  # Are we currently parsing an arithmetic expression

func newParserState(tokens: seq[Token]): ParserState =
    ParserState(tokens: tokens,
                index: 0,
                emptyExpression: Expression(kind: Empty),
                separator: none[TokenKind](),
                arithmetic: false)

func newParser*(indentation: int = 0,
                separator: TokenKind = NewLine,
                stopper: TokenKind = Indentation): Parser =
    Parser(indentation: indentation, separator: separator, stopper: stopper, error: nil)

proc addError(parser: Parser, e: ParseError) =
    if not parser.error.isNil():
        e.next = parser.error
    parser.error = e

proc nextExpression(parser: Parser,
                    state: ParserState,
                    prev: Expression = state.emptyExpression,
                    injectToken: Option[Token] = none[Token]()): Option[Expression]

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

proc nextExpression(parser: Parser,
                    state: ParserState,
                    prev: Expression = state.emptyExpression,
                    injectToken: Option[Token] = none[Token]()): Option[Expression] =
    let token = withSome injectToken:
        some tok:
            tok
        none:
            if state.index >= state.tokens.len():
                return none[Expression]()

            let tok = state.tokens[state.index]
            inc(state.index)
            tok

    withSome state.separator:
        some separator:
            if token.kind == separator:
                return some(prev)
            elif token.kind == parser.separator:
                raise newParseError(token, fmt"got {parser.separator}, but expected {separator}")
        none:
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

    of Proc:
        if state.index < state.tokens.len() and state.tokens[state.index].kind == Symbol:
            state.separator = some(SmallArrow)
            var expression = parser.nextExpression(state)
            state.separator = none[TokenKind]()

            if expression.isNone():
                raise newParseError(token, "expected a `()` expression after proc's name")
            let head = expression.get()
            if head.kind != FunctionCall:
                raise newParseError(token, "expected a `()` expression after proc's name")

            if state.index < state.tokens.len() and state.tokens[state.index].kind == Symbol:
                let typeToken = state.tokens[state.index]
                inc(state.index)

                let returnType = buildUndeterminedType(state, typeToken)

                expression = parser.nextExpression(state)
                withSome expression:
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
                raise newParseError(token, "`proc` must indicate a return type")
        else:
            raise newParseError(token, "expected a symbol after `proc`")

    of Ret:
        let expression = parser.nextExpression(state)
        if expression.isNone():
            raise newParseError(token, "expected an expression after `return`")
        return some(Expression(kind: Return, retExpr: expression.get(), token: token))

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
        let expression = parser.nextBlock(state)
        if expression.expressions.len() < 1:
            raise newParseError(token, "no block after `:`")
        return some(expression)

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
        of Ident, TypedIdent:
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

    of Not, Ampersand:
        let expression = parser.nextExpression(state)
        if expression.isNone() or expression.get().isEmpty():
            raise newParseError(token, fmt"`{token.kind}` must have an expression after it")
        return some(Expression(kind: Unary, unaryExpr: expression.get(), token: token))

    of Mul, Div, Plus, Minus, BiggerThan, BiggerThanEqual, SmallerThan, SmallerThanEqual, DoubleEqual, And, Or:
        if prev.isEmpty():
            raise newParseError(token, fmt"`{token.kind}` cannot be at the beginning of an expression")

        case prev.kind:
        of Ident, Literal, FunctionCall, BinOp, Unary:
            discard
        else:
            raise newParseError(token, fmt"left side of `{token.kind}` cannot be `{prev.kind}`")

        let isRoot = state.arithmetic == false
        state.arithmetic = true

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
            state.arithmetic = false
            return some(tree.fixedArithmeticTree())
        else:
            return some(tree)

    of Unknown:
        raise newParseError(token, fmt"unexpected token: `{token.value}`")

    else:
        raise newParseError(token, fmt"unexpected token: `{token.kind}`")
