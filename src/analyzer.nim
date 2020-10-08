import tables
import strformat
import sequtils

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

type TypeInferenceError* = object of LibraryError
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

proc newScopeFromExisting(scope: Scope): Scope =
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

proc add(scope: Scope, key: string, val: Type) =
    scope.validateUnique(key)
    scope.names[key] = val

proc add(scope: Scope, key: string, val: Function) =
    scope.validateUnique(key)
    scope.functions[key] = val

proc validate(t: Type, name: string) =
    case t.kind:
    of Auto:
        raise newException(TypeInferenceError, fmt"could not infer from a non inferred {t.kind} `{name}`")
    of Undetermined:
        raise newException(TypeInferenceError, fmt"`{name}` wasn't patched to a valid type")
    else:
        discard

proc inferType(expression: Expression, scope: Scope): Type =
    case expression.kind:
    of Ident:
        let name = expression.value
        try:
            let t = scope.names[name]
            t.validate(name)
            return t
        except KeyError:
            raise newException(TypeInferenceError, fmt"the type {name} wasn't declared yet")

    of Literal:
        return expression.literalType

    of BinOp:
        let leftType = expression.left.inferType(scope)
        let rightType = expression.right.inferType(scope)
        if leftType.kind != rightType.kind:
            raise newException(TypeInferenceError,
                               fmt"types differ on {expression.operation[]} operation, {leftType.kind} != {rightType.kind}")
        return leftType

    of FunctionCall:
        let name = expression.name
        try:
            let t = scope.functions[name].returnType
            t.validate(name)
            return t
        except:
            raise newException(TypeInferenceError, fmt"the type {name} wasn't declared yet")
    else:
        raise newException(TypeInferenceError, fmt"could not infer type from {expression[]}")

proc analyze(expression: Expression, scope: Scope) =
    case expression.kind:
    of Block:
        for e in expression.expressions:
            e.analyze(scope)

    of FunctionDeclaration:
        let name = expression.declName
        scope.validateUnique(name)

        expression.declParams.validateKind(Block)

        if expression.returnType.kind == Undetermined:
            let typeName = expression.returnType.value
            try:
                expression.returnType = scope.types[typeName]
            except KeyError:
                raise newException(AnalyzeError, fmt"the type `{typeName}` wasn't declared yet")

        let functionScope = newScopeFromExisting(scope)
        var paramsDescription = newOrderedTable[string, Type]()

        for e in expression.declParams.expressions:
            case e.kind:
            of TypedIdent:
                e.analyze(scope)
                functionScope.add(e.ident.value, e.identType)
                paramsDescription[e.ident.value] = e.identType
            of Ident:
                raise newException(AnalyzeError, fmt"`{e.value}` has no type")
            of Assign:
                raise newException(AnalyzeError, "default parameters are currently unsupported")
            else:
                raise newException(AnalyzeError, fmt"{e[]} is an invalid function parameter")

        scope.add(name, newFunction(paramsDescription, expression.returnType))
        functionScope.add(name, newFunction(paramsDescription, expression.returnType))
        expression.implementation.analyze(functionScope)

        for e in expression.implementation.expressions:
            if e.kind == Return:
                let retType = e.retExpr.inferType(scope)
                if retType.kind != expression.returnType.kind:
                    raise newException(AnalyzeError,
                                       fmt"types differ on function return `{name}`, {retType.kind} != {expression.returnType.kind}")

    of FunctionCall:
        expression.params.validateKind(Block)

        let name = expression.name
        scope.validateFunctionExists(name)

        let function = scope.functions[name]
        let paramExpressions = expression.params.expressions

        if function.params.len() != paramExpressions.len():
            raise newException(AnalyzeError,
                               fmt"`{name}` expected {function.params.len()} arguments, got {paramExpressions.len()}")

        for (paramExpr, paramType) in zip(paramExpressions, toSeq(function.params.values())):
            case paramExpr.kind:
            of Ident, Literal, FunctionCall, BinOp:
                let inferredType = paramExpr.inferType(scope)
                if paramType.kind != paramExpr.inferType(scope).kind:
                    raise newException(AnalyzeError,
                                       fmt"types differ on function call `{name}`, {paramType.kind} != {inferredType.kind}")
                paramExpr.analyze(scope)
            else:
                raise newException(AnalyzeError, fmt"{paramExpr[]} is an invalid function call parameter")

    of Declaration:
        let e = expression.declExpr

        case e.kind:
        of Assign:
            let left = e.assignee
            let right = e.assignExpr

            case left.kind:
            of Ident:
                let t = right.inferType(scope)
                scope.add(left.value, t)
                e.assignee = Expression(kind: TypedIdent, identType: t, ident: left)
            of TypedIdent:
                left.analyze(scope)
                scope.add(left.ident.value, left.identType)
            else:
                raise newException(AnalyzeError, fmt"cannot assign to {expression.assignee.kind}")

            e.analyze(scope)
        of Ident, TypedIdent:
            raise newException(AnalyzeError, "variable declaration without assignment is currently unsupported")
        else:
            raise newException(AnalyzeError, "invalid declaration of a variable")

    of TypedIdent:
        expression.ident.validateKind(Ident)
        if expression.identType.kind == Undetermined:
            let typeName = expression.identType.value
            try:
                expression.identType = scope.types[typeName]
            except KeyError:
                raise newException(AnalyzeError, fmt"the type `{typeName}` wasn't declared yet")

    of Ident:
        let name = expression.value
        if name in scope.types:
            raise newException(AnalyzeError, fmt"`{name}` is a type, not a valid variable name")

        if not (name in scope.names or name in scope.functions):
            raise newException(AnalyzeError, fmt"`{name}` wasn't declared yet")

    of BinOp:
        expression.left.analyze(scope)
        expression.right.analyze(scope)
        let _ = expression.inferType(scope)

    of Return:
        expression.retExpr.analyze(scope)

    of IfThen:
        # TODO: optimization: interpret expression to know if the function always returns false

        case expression.condition.kind:
        of BinOp, Ident, Literal:
            discard
        of FunctionCall:
            if expression.condition.inferType(scope).kind != Boolean:
                raise newException(AnalyzeError,
                                   "only boolean returning functions are valid inside an if statement currently")
        else:
            raise newException(AnalyzeError, fmt"invalid condition expression on if statement")

        if expression.then.kind != Block:
            raise newException(AnalyzeError, fmt"got {expression.then.kind}, but expected {Block}")

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
            raise newException(AnalyzeError, fmt"cannot assign from {expression.assignExpr.kind}")

    else:
        discard

proc analyze*(expression: Expression) =
    let scope = newScope()
    expression.analyze(scope)
