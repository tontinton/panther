import strformat

import stacks

import ast
import tokens

type
    InfixNode = ref object
        case operation: bool:
        of false:
            expression: Expression
        else:
            token: Token

type ArithmeticParseError* = object of LibraryError

func getOperationPriority(token: Token): int =
    case token.kind:
    of Symbol, Number, Str, True, False: 0
    of BiggerThan, BiggerThanEqual, SmallerThan, SmallerThanEqual, DoubleEqual: 1
    of Plus, Minus: 2
    of Mul, Div: 3
    else: -1

func isRightAssociative(token: Token): bool {.inline.} =
    # Currently there are no right associative operations
    false

proc addNode(stack: var Stack[Expression], operation: Token) =
    let right = stack.pop()
    let left = stack.pop()
    stack.push(Expression(kind: BinOp, left: left, operation: operation, right: right))

proc getInfix(root: Expression): seq[InfixNode] = 
    if root.kind != BinOp:
        return @[InfixNode(operation: false, expression: root)]

    result.add(InfixNode(operation: false, expression: root.left))
    result.add(InfixNode(operation: true, token: root.operation))
    for node in root.right.getInfix():
        result.add(node)

proc shuntingYard*(root: Expression): Expression =
    ## When the expression is an arithemtic expression tree,
    ## this function repairs the tree order by creating an infix list
    ## representing the arithmetic expression,
    ## then running the shunting yard algorithm on it.
    ## 
    ## see: https://en.wikipedia.org/wiki/Shunting-yard_algorithm

    if root.kind != BinOp or root.right.kind != BinOp:
        return root

    var infix = root.getInfix()

    var operatorStack = Stack[Token]()
    var operandStack = Stack[Expression]()

    for node in infix:
        if node.operation:
            let operation1 = node.token
            let priority1 = operation1.getOperationPriority()
            if priority1 == -1:
                raise newException(ArithmeticParseError, fmt"invalid arithmetic operation: `{operation1.kind}`")

            while not operatorStack.isEmpty():
                let operation2 = operatorStack.peekUnsafe()
                let priority2 = operation2.getOperationPriority()
                if ((not operation1.isRightAssociative()) and priority1 == priority2) or priority1 < priority2:
                    discard operatorStack.popUnsafe()
                    operandStack.addNode(operation2)
                else:
                    break
            operatorStack.push(operation1)
        else:
            operandStack.push(node.expression)

    while not operatorStack.isEmpty():
        operandStack.addNode(operatorStack.popUnsafe())

    return operandStack.pop()
