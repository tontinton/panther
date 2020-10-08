import tables
import strformat

import ast
import types

type
    Function = ref object
        params: OrderedTableRef[string, Type]
        returnType: Type

    Scope = ref object
        names: TableRef[string, Type]
        functions: TableRef[string, Function]
        types: TableRef[string, Type]

type AnalyzeError* = object of LibraryError

func newFunction(params: OrderedTableRef[string, Type], returnType: Type): Function =
    Function(params: params, returnType: returnType)

func newScope(): Scope =
    var types = newTable[string, Type]()
    for (key, val) in BUILTIN_TYPES:
        types[key] = Type(kind: val)

    Scope(names: newTable[string, Type](),
          functions: newTable[string, Function](),
          types: types)

func newScopeFromExisting(scope: Scope): Scope =
    var names = newTable[string, Type]()
    var functions = newTable[string, Function]()
    var types = newTable[string, Type]()

    for (key, value) in scope.names.pairs():
        names[key] = value

    for (key, value) in scope.functions.pairs():
        functions[key] = value

    for (key, value) in scope.types.pairs():
        types[key] = value

    Scope(names: names, functions: functions, types: types)

func validateUnique(scope: Scope, name: string) =
    if name in scope.names:
        raise newException(AnalyzeError, fmt"`{name}` is already declared as a variable")
    if name in scope.functions:
        raise newException(AnalyzeError, fmt"`{name}` is already declared as a function")
    if name in scope.types:
        raise newException(AnalyzeError, fmt"`{name}` is already declared as a type")

func validateFunctionExists(scope: Scope, name: string) =
    if not (name in scope.functions):
        raise newException(AnalyzeError, fmt"the function `{name}` wasn't declared yet")

func validateKind(expression: Expression, kind: ExpressionKind) =
    if expression.kind != kind:
        raise newException(AnalyzeError, fmt"got `{kind}`, but expected `{expression.kind}`")

proc `[]=`(scope: Scope, key: string, val: Type) =
    scope.validateUnique(key)
    scope.names[key] = val

proc `[]=`(scope: Scope, key: string, val: Function) =
    scope.validateUnique(key)
    scope.functions[key] = val

proc analyze(expression: Expression, scope: Scope) =
    case expression.kind:
    of Block:
        for e in expression.expressions:
            e.analyze(scope)

    of FunctionDeclaration:
        let name = expression.declName
        scope.validateUnique(name)

        expression.declParams.validateKind(Block)

        let functionScope = newScopeFromExisting(scope)
        var paramsDescription = newOrderedTable[string, Type]()

        for e in expression.declParams.expressions:
            case e.kind:
            of TypedIdent:
                e.ident.validateKind(Ident)
                functionScope[e.ident.value] = e.identType
                paramsDescription[e.ident.value] = e.identType
            of Ident:
                raise newException(AnalyzeError, fmt"`{e.value}` has no type")
            of Assign:
                raise newException(AnalyzeError, "default parameters are currently unsupported")
            else:
                raise newException(AnalyzeError, fmt"{e[]} is an invalid function parameter")

        scope[name] = newFunction(paramsDescription, expression.returnType)
        expression.implementation.analyze(functionScope)

    of FunctionCall:
        expression.params.validateKind(Block)
        scope.validateFunctionExists(expression.name)

        let function = scope.functions[expression.name]
        let paramExpressions = expression.params.expressions

        if function.params.len() != paramExpressions.len():
            raise newException(AnalyzeError,
                               fmt"`{expression.name}` expected {function.params.len()} arguments, got {paramExpressions.len()}")

        for e in paramExpressions:
            case e.kind:
            of Ident, Literal, FunctionCall, BinOp:
                # TODO: validate types
                e.analyze(scope)
            else:
                raise newException(AnalyzeError, fmt"{e[]} is an invalid function call parameter")

    of Declaration:
        let e = expression.declExpr

        case e.kind:
        of Assign:
            # TODO: type inference
            case e.assignee.kind:
            of Ident:
                scope[e.assignee.value] = Type(kind: Auto)
            of TypedIdent:
                e.assignee.ident.validateKind(Ident)
                scope[e.assignee.ident.value] = e.assignee.identType
            else:
                raise newException(AnalyzeError, fmt"cannot assign to {expression.assignee.kind}")

            e.analyze(scope)
        of Ident, TypedIdent:
            raise newException(AnalyzeError, "variable declaration without assignment is currently unsupported")
        else:
            raise newException(AnalyzeError, "invalid declaration of a variable")

    of TypedIdent:
        expression.ident.validateKind(Ident)

    of Ident:
        let name = expression.value
        if name in scope.types:
            raise newException(AnalyzeError, fmt"`{name}` is a type, not a valid variable name")

        if not (name in scope.names or name in scope.functions):
            raise newException(AnalyzeError, fmt"`{name}` wasn't declared yet")

    of BinOp:
        expression.left.analyze(scope)
        expression.right.analyze(scope)

    of IfThen:
        expression.condition.analyze(scope)
        expression.then.analyze(scope)

    of IfElseThen:
        expression.ifThen.analyze(scope)
        expression.otherwise.analyze(scope)

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
            raise newException(AnalyzeError, fmt"cannot assign from {expression.assignee.kind}")

    else:
        discard

proc analyze*(expression: Expression) =
    let scope = newScope()
    expression.analyze(scope)
