import options
import strformat

import safeoptions

import ast
import tokens
import types
import arithmetic

type
    Parser* = ref object
        indentation: int
        separator: TokenKind
        stopper: TokenKind

    ParserState = ref object
        tokens: seq[Token]
        index: int
        emptyExpression: Expression
        separator: Option[TokenKind]
        arithmetic: bool  # Are we currently parsing an arithmetic expression

type ParseError* = object of LibraryError

func newParserState(tokens: seq[Token]): ParserState =
    ParserState(tokens: tokens,
                index: 0,
                emptyExpression: Expression(kind: Empty),
                separator: none[TokenKind](),
                arithmetic: false)

func newParser*(indentation: int = 0,
                separator: TokenKind = NewLine,
                stopper: TokenKind = Indentation): Parser =
    Parser(indentation: indentation, separator: separator, stopper: stopper)

proc nextExpression(parser: Parser,
                    state: ParserState,
                    prev: Expression = state.emptyExpression,
                    injectToken: Option[Token] = none[Token]()): Option[Expression]

iterator parse(parser: Parser, state: ParserState): Expression =
    var expression = parser.nextExpression(state)
    while expression.isSome():
        yield expression.get()
        expression = parser.nextExpression(state)

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
    for expression in blockParser.parse(state):
        result.expressions.add(expression)

proc nextHead(parser: Parser, state: ParserState): Expression =
    let blockParser = newParser(separator = Comma, stopper = CloseBracket)
    result = Expression(kind: Block, expressions: @[])
    for expression in blockParser.parse(state):
        result.expressions.add(expression)

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
                raise newException(ParseError, fmt"got {parser.separator}, but expected {separator}")
        none:
            if token.kind == parser.separator:
                return some(prev)

    case token.kind:
    of Indentation:
        if parser.stopper != Indentation or token.length == parser.indentation:
            return parser.nextExpression(state, prev)
        elif token.length < parser.indentation:
            dec(state.index)
            return none[Expression]()
        else:
            raise newException(ParseError, "indentation of line cannot be larger without starting a new block")

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
            raise newException(ParseError, "expected a symbol after `let`")
        return some(Expression(kind: Declaration,
                               declExpr: expression.get()))

    of Colon:
        if prev.kind != Ident:
            raise newException(ParseError, "expected `:` only after a symbol")
        else:
            if state.index < state.tokens.len() and state.tokens[state.index].kind == Symbol:
                let typeToken = state.tokens[state.index]
                inc(state.index)

                return parser.nextExpression(state, Expression(kind: TypedIdent,
                                                               identType: Type(kind: Undetermined, value: typeToken.value),
                                                               ident: prev))
            else:
                raise newException(ParseError, "expected a type after `:`")

    of Proc:
        if state.index < state.tokens.len() and state.tokens[state.index].kind == Symbol:
            state.separator = some(SmallArrow)
            var expression = parser.nextExpression(state)
            state.separator = none[TokenKind]()

            if expression.isNone():
                raise newException(ParseError, "expected a `()` expression after proc's name")
            let head = expression.get()
            if head.kind != FunctionCall:
                raise newException(ParseError, "expected a `()` expression after proc's name")

            if state.index < state.tokens.len() and state.tokens[state.index].kind == Symbol:
                let typeToken = state.tokens[state.index]
                inc(state.index)

                expression = parser.nextExpression(state)
                withSome expression:
                    some procBlock:
                        return some(Expression(kind: FunctionDeclaration,
                                                declName: head.name,
                                                declParams: head.params,
                                                returnType: Type(kind: Undetermined, value: typeToken.value),
                                                implementation: procBlock))
                    none:
                        raise newException(ParseError, fmt"expected an implementation for {head.name}")
            else:
                raise newException(ParseError, "`proc` must indicate a return type")
        else:
            raise newException(ParseError, "expected a symbol after `proc`")

    of Ret:
        let expression = parser.nextExpression(state)
        if expression.isNone():
            raise newException(ParseError, "expected an expression after `return`")
        return some(Expression(kind: Return, retExpr: expression.get()))

    of OpenBracket:
        if prev.kind == Ident:
            let call = Expression(kind: FunctionCall, name: prev.value, params: parser.nextHead(state))
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
            raise newException(ParseError, "unexpected `)`")

    of Symbol:
        return parser.nextExpression(state, Expression(kind: Ident, value: token.value))

    of Number:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: Signed32),
                                                       literal: token.value))

    of Str:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: String),
                                                       literal: token.value))

    of True:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: Boolean),
                                                       literal: "true"))

    of False:
        return parser.nextExpression(state, Expression(kind: Literal,
                                                       literalType: Type(kind: Boolean),
                                                       literal: "false"))

    of ColonNewLine:
        let expression = parser.nextBlock(state)
        if expression.expressions.len() < 1:
            raise newException(ParseError, "no block after `:`")
        return some(expression)

    of If, ElseIf:
        if token.kind == ElseIf and prev.kind != IfThen:
            raise newException(ParseError, "`elif` can only come after `if`")

        state.separator = some(ColonNewLine)
        var expression = parser.nextExpression(state)
        state.separator = none[TokenKind]()

        if expression.isNone():
            raise newException(ParseError, fmt"no expression after `{token.kind}`")

        let condition = expression.get()

        dec(state.index)
        expression = parser.nextExpression(state)
        if expression.isNone():
            raise newException(ParseError, fmt"no block after `{token.kind}`")

        let then = expression.get()
        let ifExpr = Expression(kind: IfThen, condition: condition, then: then)

        if state.index + 1 < state.tokens.len() and state.tokens[state.index].kind == Indentation:
            let nextToken = state.tokens[state.index + 1]
            case nextToken.kind:
            of ElseIf:
                expression = parser.nextExpression(state, ifExpr)
                if expression.isNone():
                    raise newException(ParseError, fmt"no block after `{nextToken.kind}`")

                let elseExpr = expression.get()
                return some(Expression(kind: IfElseThen, ifThen: ifExpr, otherwise: elseExpr))
            of Else:
                return parser.nextExpression(state, ifExpr)
            else:
                return some(ifExpr)
        else:
            return some(ifExpr)

    of Else:
        if prev.kind != IfThen:
            raise newException(ParseError, "`else` can only come after `if`")

        let expression = parser.nextExpression(state)
        if expression.isNone():
            raise newException(ParseError, "no expression after `else`")

        let elseExpr = expression.get()
        if elseExpr.kind != Block:
            raise newException(ParseError, fmt"expected a block, not `{elseExpr.kind}`")

        return some(Expression(kind: IfElseThen, ifThen: prev, otherwise: elseExpr))

    of Equal:
        case prev.kind:
        of Ident, TypedIdent:
            discard
        else:
            raise newException(ParseError, "left side of `=` must be a valid assignment expression")

        let expression = parser.nextExpression(state)
        if expression.isNone() or expression.get().isEmpty():
            raise newException(ParseError, "`=` must have an expression after it")

        let subtree = expression.get()
        case subtree.kind:
        of BinOp, Ident, Literal, FunctionCall:
            return some(Expression(kind: Assign, assignee: prev, assignExpr: subtree))
        else:
            raise newException(ParseError, "right side of `=` must be a valid arithmetic expression")

    of Mul, Div, Plus, Minus, BiggerThan, BiggerThanEqual, SmallerThan, SmallerThanEqual, DoubleEqual, And, Or:
        if prev.isEmpty():
            raise newException(ParseError, fmt"`{token.kind}` cannot be at the beginning of an expression")

        case prev.kind:
        of Ident, Literal, FunctionCall, BinOp:
            discard
        else:
            raise newException(ParseError, fmt"left side of `{token.kind}` cannot be `{prev.kind}`")

        let isRoot = state.arithmetic == false
        state.arithmetic = true

        let expression = parser.nextExpression(state)
        if expression.isNone() or expression.get().isEmpty():
            raise newException(ParseError, fmt"`{token.kind}` must have an expression after it")

        let subtree = expression.get()

        let tree = case subtree.kind:
        of BinOp:
            let newRight = Expression(kind: BinOp, left: subtree.left, operation: subtree.operation, right: subtree.right)
            Expression(kind: BinOp, left: prev, operation: token, right: newRight)
        of Ident, Literal, FunctionCall:
            Expression(kind: BinOp, left: prev, operation: token, right: subtree)
        else:
            raise newException(ParseError, 
                               fmt"right side of `{token.kind}` cannot be `{subtree.kind}`")

        if isRoot:
            state.arithmetic = false
            return some(tree.getFixedArithmeticTree())
        else:
            return some(tree)

    else:
        raise newException(ParseError, fmt"unexpected token: {token[]}")
