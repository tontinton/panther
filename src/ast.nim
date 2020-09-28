import strutils
import strformat

import tokens
import types

type
    ExpressionKind* = enum
        Empty
        Ident
        TypedIdent
        Literal
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
        case kind*: ExpressionKind
        of Empty:
            discard
        of Ident, Literal:
            value*: string
        of TypedIdent:
            identType*: Type
            ident*: Expression
        of BinOp:
            left*: Expression
            operation*: Token
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

func formatTreeString(expression: Expression, tabs: uint): string =
    case expression.kind:
    of Empty:
        fmt"{tabs.toString()}empty"

    of Ident:
        fmt"{tabs.toString()}ident: {expression.value}"

    of TypedIdent:
        fmt"""{tabs.toString()}type: {expression.identType[]}
{expression.ident.formatTreeString(tabs)}"""

    of Literal:
        fmt"{tabs.toString()}literal: {expression.value}"

    of BinOp:
        let operator = case expression.operation.kind:
                       of Plus: "+"
                       of Minus: "-"
                       of Mul: "*"
                       of Div: "/"
                       of BiggerThan: ">"
                       of BiggerThanEqual: ">="
                       of SmallerThan: "<"
                       of SmallerThanEqual: "<="
                       of DoubleEqual: "=="
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
            block_string.add(fmt"""

  {t}(
{inner.formatTreeString(tabs + 2)}
  {t}),""")
        fmt"""{t}[{block_string}
{t}]"""

    of Return:
        fmt"""{tabs.toString()}return:
{expression.retExpr.formatTreeString(tabs + 1)}"""

func `$`*(expression: Expression): string =
    formatTreeString(expression, 0)
