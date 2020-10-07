import tables
import strformat

import ast
import types

type
    Scope* = ref object
        names: TableRef[string, Type]

type AnalyzeError* = object of LibraryError

func newScope(): Scope =
    Scope(names: newTable[string, Type]())

func newScopeFromExisting(scope: Scope): Scope =
    var names = newTable[string, Type]()

    for (key, value) in scope.names.pairs():
        names[key] = value

    Scope(names: names)

func validateUnique(scope: Scope, name: string) =
    if name in scope.names:
        raise newException(AnalyzeError, fmt"{name} is already declared")

func validateExists(scope: Scope, name: string) =
    if not (name in scope.names):
        raise newException(AnalyzeError, fmt"{name} wasn't declared yet")

func validateKind(expression: Expression, kind: ExpressionKind) =
    if expression.kind != kind:
        raise newException(AnalyzeError, fmt"got {kind}, but expected {expression.kind}")

proc `[]=`(scope: Scope, key: string, val: Type) =
    scope.validateUnique(key)
    scope.names[key] = val

proc analyze(expression: Expression, scope: Scope) =
    case expression.kind:
    of Block:
        for e in expression.expressions:
            e.analyze(scope)

    of FunctionDeclaration:
        let name = expression.declName
        scope.validateUnique(name)

        let params = expression.declParams
        params.validateKind(Block)

        scope[name] = expression.returnType

        let functionScope = newScopeFromExisting(scope)

        for e in params.expressions:
            case e.kind:
            of TypedIdent:
                e.ident.validateKind(Ident)
                functionScope[e.ident.value] = e.identType
                e.analyze(functionScope)
            of Ident:
                raise newException(AnalyzeError, fmt"{e.value} has no type")
            of Assign:
                raise newException(AnalyzeError, "default parameters are currently unsupported")
            else:
                raise newException(AnalyzeError, fmt"{$e} is an invalid function parameter")

        expression.implementation.analyze(functionScope)

    of FunctionCall:
        # TODO: validate parameters and types
        scope.validateExists(expression.name)

    of Declaration:
        let e = expression.declExpr

        case e.kind:
        of Assign:
            # TODO: type inference
            case e.assignee.kind:
            of Ident:
                scope[e.assignee.value] = Type(kind: Auto)
                e.assignee.analyze(scope)
            of TypedIdent:
                e.assignee.ident.validateKind(Ident)
                scope.validateUnique(e.assignee.ident.value)
                scope[e.assignee.ident.value] = e.assignee.identType
                e.assignee.analyze(scope)
            else:
                raise newException(AnalyzeError, fmt"cannot assign to {expression.assignee.kind}")

            e.analyze(scope)
        of Ident, TypedIdent:
            raise newException(AnalyzeError, "variable declaration without assignment is currently unsupported")
        else:
            raise newException(AnalyzeError, "invalid declaration of a variable")

    of TypedIdent:
        let ident = expression.ident
        ident.validateKind(Ident)
        ident.analyze(scope)

    of Ident:
        scope.validateExists(expression.value)

    of Assign:
        case expression.assignee.kind:
        of Ident, TypedIdent:
            expression.assignee.analyze(scope)
        else:
            raise newException(AnalyzeError, fmt"cannot assign to {expression.assignee.kind}")

        case expression.assignExpr.kind:
        of BinOp, Ident, Literal, FunctionCall:
            expression.assignExpr.analyze(scope)
        else:
            raise newException(AnalyzeError, fmt"cannot assign to {expression.assignee.kind}")

    else:
        discard

proc analyze*(expression: Expression) =
    let scope = newScope()
    expression.analyze(scope)
