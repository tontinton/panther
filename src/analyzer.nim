import tables
import strformat
import sequtils
import options

import optionsutils

import ast
import types
import tokens
import frontenderrors

type
    Function = ref object
        params: OrderedTableRef[string, Type]
        returnType: Type

    Scope = ref object
        names: TableRef[string, Type]
        functions: TableRef[string, Function]
        types: TableRef[string, Type]
        insideFunction: Option[string]

func newFunction(params: OrderedTableRef[string, Type], returnType: Type): Function =
    Function(params: params, returnType: returnType)

func newGlobalScope(): Scope =
    var types = newTable[string, Type]()
    for (key, val) in BUILTIN_TYPES:
        types[key] = Type(kind: val)

    Scope(names: newTable[string, Type](),
          functions: newTable[string, Function](),
          types: types,
          insideFunction: none[string]())

proc newFunctionScope(scope: Scope, funcName: string): Scope =
    var names = newTable[string, Type]()
    var functions = newTable[string, Function]()
    var types = newTable[string, Type]()

    for (key, value) in scope.names.pairs():
        names[key] = value

    for (key, value) in scope.functions.pairs():
        functions[key] = value

    for (key, value) in scope.types.pairs():
        types[key] = value

    Scope(names: names, functions: functions, types: types, insideFunction: some(funcName))

proc inferType(expression: Expression, scope: Scope): Type =
    proc validate(t: Type, name: string) =
        case t.kind:
        of Auto:
            raise newParseError(expression, fmt"could not infer from a non inferred {t.kind} `{name}`")
        of Undetermined:
            raise newParseError(expression, fmt"`{name}` wasn't patched to a valid type")
        else:
            discard

    case expression.kind:
    of Ident:
        let name = expression.value
        try:
            let t = scope.names[name]
            t.validate(name)
            return t
        except KeyError:
            raise newParseError(expression, fmt"the type {name} wasn't declared yet")

    of Literal:
        return expression.literalType

    of BinOp:
        let leftType = expression.left.inferType(scope)
        let rightType = expression.right.inferType(scope)
        if leftType.kind != rightType.kind:
            raise newParseError(expression,
                                fmt"types differ on {expression.operation[]} operation, {leftType.kind} != {rightType.kind}")

        case expression.operation.kind:
        of BiggerThan, BiggerThanEqual, SmallerThan, SmallerThanEqual, DoubleEqual, And, Or:
            return Type(kind: Boolean)
        else:
            return leftType

    of FunctionCall:
        let name = expression.name
        try:
            let t = scope.functions[name].returnType
            t.validate(name)
            return t
        except:
            raise newParseError(expression, fmt"the type {name} wasn't declared yet")
    else:
        raise newParseError(expression, fmt"could not infer type from {expression[]}")

proc analyze(expression: Expression, scope: Scope) =
    var errors : ParseError = nil

    proc addError(e: ParseError) =
        if not errors.isNil():
            e.next = errors
        errors = e

    func validateUnique(scope: Scope, name: string) =
        if name in scope.names:
            raise newParseError(expression, fmt"`{name}` is already declared as a variable")
        if name in scope.functions:
            raise newParseError(expression, fmt"`{name}` is already declared as a function")
        if name in scope.types:
            raise newParseError(expression, fmt"`{name}` is already declared as a type")

    func validateFunctionExists(scope: Scope, name: string) =
        if not (name in scope.functions):
            raise newParseError(expression, fmt"the function `{name}` wasn't declared yet")

    func validateKind(expression: Expression, kind: ExpressionKind) =
        if expression.kind != kind:
            raise newParseError(expression, fmt"got `{kind}`, but expected `{expression.kind}`")

    proc add(scope: Scope, key: string, val: Type) =
        scope.validateUnique(key)
        scope.names[key] = val

    proc add(scope: Scope, key: string, val: Function) =
        scope.validateUnique(key)
        scope.functions[key] = val

    try:
        case expression.kind:
        of Block:
            for exp in expression.expressions:
                try:
                    exp.analyze(scope)
                except ParseError as e:
                    addError(e)

        of FunctionDeclaration:
            let name = expression.declName

            scope.validateUnique(name)

            expression.declParams.validateKind(Block)

            if expression.returnType.kind == Undetermined:
                let typeName = expression.returnType.value
                try:
                    expression.returnType = scope.types[typeName]
                except KeyError:
                    raise newParseError(expression, fmt"the type `{typeName}` wasn't declared yet")

            let functionScope = newFunctionScope(scope, name)
            var paramsDescription = newOrderedTable[string, Type]()

            for exp in expression.declParams.expressions:
                case exp.kind:
                of TypedIdent:
                    exp.analyze(scope)
                    functionScope.add(exp.ident.value, exp.identType)
                    paramsDescription[exp.ident.value] = exp.identType
                of Ident:
                    raise newParseError(expression, fmt"`{exp.value}` has no type")
                of Assign:
                    raise newParseError(expression, "default parameters are currently unsupported")
                else:
                    raise newParseError(expression, fmt"{exp[]} is an invalid function parameter")

            let funcDescription = newFunction(paramsDescription, expression.returnType)
            scope.add(name, funcDescription)
            functionScope.add(name, funcDescription)
    
            expression.implementation.analyze(functionScope)

        of Return:
            withSome scope.insideFunction:
                some name:
                    try:
                        expression.retExpr.analyze(scope)
                    except ParseError as e:
                        addError(e)

                    scope.validateFunctionExists(name)

                    let retType = scope.functions[name].returnType
                    let inferredType = expression.retExpr.inferType(scope)
                    if inferredType.kind != retType.kind:    
                        # TODO: add the function declaration expression to the error message somehow
                        raise newParseError(expression,
                                            fmt"return type of `{name}` differs, {retType.kind} != {inferredType.kind}")
                none:
                    raise newParseError(expression, "cannot return from global scope")

        of FunctionCall:
            expression.params.validateKind(Block)

            let name = expression.name
            scope.validateFunctionExists(name)

            let function = scope.functions[name]
            let paramExpressions = expression.params.expressions

            if function.params.len() != paramExpressions.len():
                # TODO: add the function declaration expression to the error message somehow
                raise newParseError(expression,
                                    fmt"`{name}` expected {function.params.len()} arguments, got {paramExpressions.len()}")

            for (paramExpr, paramType) in zip(paramExpressions, toSeq(function.params.values())):
                case paramExpr.kind:
                of Ident, Literal, FunctionCall, BinOp:
                    let inferredType = paramExpr.inferType(scope)
                    if paramType.kind != paramExpr.inferType(scope).kind:
                        raise newParseError(expression,
                                            fmt"types differ on function call `{name}`, {paramType.kind} != {inferredType.kind}")
                    paramExpr.analyze(scope)
                else:
                    raise newParseError(expression, fmt"{paramExpr[]} is an invalid function call parameter")

        of Declaration:
            let declExpr = expression.declExpr

            case declExpr.kind:
            of Assign:
                let left = declExpr.assignee
                let right = declExpr.assignExpr

                case left.kind:
                of Ident:
                    let t = right.inferType(scope)
                    scope.add(left.value, t)
                    declExpr.assignee = Expression(kind: TypedIdent, identType: t, ident: left)
                of TypedIdent:
                    left.analyze(scope)
                    scope.add(left.ident.value, left.identType)
                else:
                    raise newParseError(expression, fmt"cannot assign to {expression.assignee.kind}")

                declExpr.analyze(scope)
            of Ident, TypedIdent:
                raise newParseError(expression, "variable declaration without assignment is currently unsupported")
            else:
                raise newParseError(expression, "invalid declaration of a variable")

        of TypedIdent:
            expression.ident.validateKind(Ident)
            if expression.identType.kind == Undetermined:
                let typeName = expression.identType.value
                try:
                    expression.identType = scope.types[typeName]
                except KeyError:
                    raise newParseError(expression, fmt"the type `{typeName}` wasn't declared yet")

        of Ident:
            let name = expression.value
            if name in scope.types:
                raise newParseError(expression, fmt"`{name}` is a type, not a valid variable name")

            if not (name in scope.names or name in scope.functions):
                raise newParseError(expression, fmt"`{name}` wasn't declared yet")

        of BinOp:
            expression.left.analyze(scope)
            expression.right.analyze(scope)
            discard expression.inferType(scope)

        of IfThen:
            # TODO: optimization: interpret expression to know if the function always returns false

            try:
                case expression.condition.kind:
                of BinOp, Ident, Literal:
                    discard
                of FunctionCall:
                    if expression.condition.inferType(scope).kind != Boolean:
                        raise newParseError(expression,
                                            "only boolean returning functions are valid inside an if statement currently")
                else:
                    raise newParseError(expression, fmt"invalid condition expression on if statement")

                if expression.then.kind != Block:
                    raise newParseError(expression, fmt"got {expression.then.kind}, but expected {Block}")

                expression.condition.analyze(scope)
            except ParseError as e:
                addError(e)

            expression.then.analyze(scope)

        of IfElseThen:
            expression.ifThen.analyze(scope)
            expression.otherwise.analyze(scope)

        of Assign:
            case expression.assignee.kind:
            of Ident, TypedIdent:
                expression.assignee.analyze(scope)
            else:
                raise newParseError(expression, fmt"cannot assign to {expression.assignee.kind}")

            case expression.assignExpr.kind:
            of BinOp, Ident, Literal, FunctionCall:
                expression.assignExpr.analyze(scope)
            else:
                raise newParseError(expression, fmt"cannot assign from {expression.assignExpr.kind}")

        else:
            discard
    except ParseError as e:
        addError(e)

    if not errors.isNil():
        raise errors

proc analyze*(expression: Expression) =
    let scope = newGlobalScope()
    expression.analyze(scope)
