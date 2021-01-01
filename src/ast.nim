import strutils
import strformat

import tokens
import types
import customerrors

type
    ExpressionKind* = enum
        Empty
        Ident
        TypedIdent
        Literal
        Unary
        BinOp
        IfThen
        IfElseThen
        Assign
        Declaration
        FunctionDeclaration
        FunctionCall
        Return
        Block

    Expression* = ref object
        token*: Token  # Used for error messages

        case kind*: ExpressionKind
        of Empty:
            discard
        of Ident:
            value*: string
        of Literal:
            literalType*: Type
            literal*: string
        of TypedIdent:
            identType*: Type
            ident*: Expression
        of Unary:
            unaryExpr*: Expression
        of BinOp:
            left*: Expression
            right*: Expression
        of IfThen:
            condition*: Expression
            then*: Expression
        of IfElseThen:
            ifThen*: Expression
            otherwise*: Expression
        of Assign:
            assignee*: Expression
            assignExpr*: Expression
        of Declaration:
            declExpr*: Expression
        of FunctionDeclaration:
            declName*: string
            declParams*: Expression
            returnType*: Type
            implementation*: Expression
        of FunctionCall:
            name*: string
            params*: Expression
        of Return:
            retExpr*: Expression
        of Block:
            expressions*: seq[Expression]

func isEmpty*(expression: Expression): bool =
    expression.kind == Empty

func toString(tabs: uint): string =
    "  ".repeat(tabs)

func formatTreeString(expression: Expression, tabs: uint = 0): string =
    case expression.kind:
    of Empty:
        fmt"{tabs.toString()}empty"

    of Ident:
        fmt"{tabs.toString()}ident: {expression.value}"

    of TypedIdent:
        &"{tabs.toString()}type: {expression.identType[]}\n{expression.ident.formatTreeString(tabs)}"

    of Literal:
        let t = tabs.toString()
        &"{tabs.toString()}type: {expression.literalType[]}\n{t}literal: {expression.literal}"

    of Unary:
        let operator = case expression.token.kind:
                       of Not: "not"
                       else: "?"
        let t = tabs.toString()
        fmt"""{t}{operator}:
{expression.unaryExpr.formatTreeString(tabs + 1)}"""

    of BinOp:
        let operator = case expression.token.kind:
                       of Plus: "+"
                       of Minus: "-"
                       of Mul: "*"
                       of Div: "/"
                       of BiggerThan: ">"
                       of BiggerThanEqual: ">="
                       of SmallerThan: "<"
                       of SmallerThanEqual: "<="
                       of DoubleEqual: "=="
                       of And: "and"
                       of Or: "or"
                       else: "?"
        let t = tabs.toString()
        fmt"""{t}{operator}:
  {t}left:
{expression.left.formatTreeString(tabs + 2)}
  {t}right:
{expression.right.formatTreeString(tabs + 2)}"""

    of IfThen:
        let t = tabs.toString()
        fmt"""{t}if:
{expression.condition.formatTreeString(tabs + 1)}
{t}then:
{expression.then.formatTreeString(tabs + 1)}"""

    of IfElseThen:
        let t = tabs.toString()
        fmt"""{expression.ifThen.formatTreeString(tabs)}
{t}else:
{expression.otherwise.formatTreeString(tabs + 1)}"""

    of Assign:
        let t = tabs.toString()
        fmt"""{t}=:
  {t}asignee:
{expression.assignee.formatTreeString(tabs + 2)}
  {t}value:
{expression.assignExpr.formatTreeString(tabs + 2)}"""

    of Declaration:
        let t = tabs.toString()
        fmt"""{t}declaration:
{expression.declExpr.formatTreeString(tabs + 1)}"""

    of FunctionDeclaration:
        let t = tabs.toString()
        fmt"""{t}function:
  {t}name: {expression.declName}
  {t}params:
{expression.declParams.formatTreeString(tabs + 2)}
  {t}return type: {expression.returnType[]}
  {t}implementation:
{expression.implementation.formatTreeString(tabs + 2)}"""

    of FunctionCall:
        let t = tabs.toString()
        fmt"""{t}function call:
  {t}name: {expression.name}
  {t}params:
{expression.params.formatTreeString(tabs + 2)}"""

    of Block:
        var block_string = ""
        let t = tabs.toString()
        for inner in expression.expressions:
            block_string.add(&"\n  {t}(\n{inner.formatTreeString(tabs + 2)}\n  {t}),")
        &"{t}[{block_string}\n{t}]"

    of Return:
        &"{tabs.toString()}return:\n{expression.retExpr.formatTreeString(tabs + 1)}"

func `$`*(expression: Expression): string =
    expression.formatTreeString()

proc error*(expression: Expression): seq[ErrorInfo] =
    case expression.kind:
    of Block:
        for subExpression in expression.expressions:
            for error in subExpression.error:
                result.add(error)
    else:
        result.add(expression.token.error)
