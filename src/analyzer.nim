import tables
import strformat
import sequtils
import options
import macros

import optionsutils

import ast
import types
import tokens
import customerrors

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
        types[key] = Type(kind: val, ptrLevel: 0)

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
    proc validateType(t: Type, name: string) =
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
            t.validateType(name)
            return t
        except KeyError:
            raise newParseError(expression, fmt"the variable `{name}` wasn't declared yet")

    of TypedIdent:
        return expression.identType

    of Literal:
        return expression.literalType

    of Unary:
        case expression.token.kind:
        of Not:
            return Type(kind: Boolean)
        of Ampersand:
            return expression.unaryExpr.inferType(scope).reference()
        of Mul:
            let t = expression.unaryExpr.inferType(scope)
            if t.ptrLevel == 0:
                raise newParseError(expression, fmt"cannot dereference a non ptr type")
            return t.dereference()
        else:
            raise newParseError(expression, fmt"cannot infer {expression.token.kind} in a unary expression")

    of BinOp:
        let leftType = expression.left.inferType(scope)
        let rightType = expression.right.inferType(scope)
        if leftType != rightType:
            raise newParseError(expression,
                                fmt"types differ on {expression.token.kind} operation, {leftType} != {rightType}")

        case expression.token.kind:
        of BiggerThan, BiggerThanEqual, SmallerThan, SmallerThanEqual, DoubleEqual, And, Or:
            return Type(kind: Boolean)
        else:
            return leftType

    of FunctionCall:
        let name = expression.name
        try:
            let t = scope.functions[name].returnType
            t.validateType(name)
            return t
        except KeyError:
            raise newParseError(expression, fmt"the function {name} wasn't declared yet")

    of Assign:
        let leftType = expression.assignee.inferType(scope)
        let rightType = expression.assignExpr.inferType(scope)
        if leftType != rightType:
            raise newParseError(expression,
                                fmt"types differ on {expression.token.kind} operation, {leftType} != {rightType}")
        return leftType

    else:
        raise newParseError(expression, fmt"could not infer type from {expression[]}")

proc analyze(expression: Expression, scope: Scope) =
    var errors : ParseError = nil

    proc addError(e: ParseError) =
        if not errors.isNil():
            e.next = errors
        errors = e

    macro withErrorCatching(statement: untyped): untyped =
        quote do:
            try:
                `statement`
            except ParseError as e:
                addError(e)

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

    withErrorCatching:
        case expression.kind:
        of Block:
            for exp in expression.expressions:
                withErrorCatching:
                    exp.analyze(scope)

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
                    withErrorCatching:
                        expression.retExpr.analyze(scope)

                    scope.validateFunctionExists(name)

                    let retType = scope.functions[name].returnType
                    let inferredType = expression.retExpr.inferType(scope)
                    if inferredType != retType:    
                        # TODO: add the function declaration expression to the error message somehow
                        raise newParseError(expression,
                                            fmt"return type of `{name}` differs, {retType} != {inferredType}")
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
                of Ident, Literal, FunctionCall, BinOp, Unary:
                    let inferredType = paramExpr.inferType(scope)
                    if paramType != inferredType:
                        raise newParseError(expression,
                                            fmt"types differ on function call `{name}`, {paramType} != {inferredType}")
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
                    declExpr.assignee = Expression(kind: TypedIdent,
                                                   identType: t,
                                                   ident: left,
                                                   token: left.token)
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
                let ptrLevel = expression.identType.ptrLevel
                try:
                    expression.identType = Type(kind: scope.types[typeName].kind,
                                                ptrLevel:ptrLevel)
                except KeyError:
                    raise newParseError(expression, fmt"the type `{typeName}` wasn't declared yet")

        of Ident:
            let name = expression.value
            if name in scope.types:
                raise newParseError(expression, fmt"`{name}` is a type, not a valid variable name")

            if not (name in scope.names or name in scope.functions):
                raise newParseError(expression, fmt"`{name}` wasn't declared yet")

        of Unary:
            let leftKind = expression.token.kind
            let right = expression.unaryExpr
            let rightKind = right.kind

            case leftKind:
            of Ampersand, Mul:
                case rightKind:
                of Ident:
                    right.analyze(scope)
                of Unary:
                    if leftKind == Ampersand:
                        if right.token.kind == Ampersand:
                            raise newParseError(expression, fmt"cannot have a reference of a reference")
                        else:
                            raise newParseError(expression,
                                                fmt"{right.token.kind} to the right of {leftKind} is currently unsupported")
                    right.analyze(scope)
                else:
                    raise newParseError(expression, fmt"{rightKind} is not a valid right side of {leftKind}")
            else:
                case rightKind:
                of Ident, Literal, FunctionCall, BinOp, Unary:
                    right.analyze(scope)
                else:
                    raise newParseError(expression, fmt"invalid right side of {leftKind}")

        of BinOp:
            case expression.left.kind:
            of Ident, Literal, FunctionCall, BinOp, Unary:
                expression.left.analyze(scope)
            else:
                raise newParseError(expression, fmt"invalid left side of {expression.token.kind}")

            case expression.right.kind:
            of Ident, Literal, FunctionCall, BinOp, Unary:
                expression.right.analyze(scope)
            else:
                raise newParseError(expression, fmt"invalid right side of {expression.token.kind}")

            discard expression.inferType(scope)

        of IfThen:
            withErrorCatching:
                case expression.condition.kind:
                of Ident, Literal, FunctionCall, BinOp, Unary:
                    discard
                else:
                    raise newParseError(expression, fmt"invalid condition expression on if statement")

                if expression.then.kind != Block:
                    raise newParseError(expression, fmt"got {expression.then.kind}, but expected {Block}")

                expression.condition.analyze(scope)

            expression.then.analyze(scope)

        of IfElseThen:
            expression.ifThen.analyze(scope)
            expression.otherwise.analyze(scope)

        of Assign:
            let assignee = expression.assignee
            case assignee.kind:
            of Ident, TypedIdent:
                assignee.analyze(scope)
            of Unary:
                if assignee.token.kind != Mul:
                    raise newParseError(expression, fmt"cannot assign to unary {assignee.kind}")
                assignee.analyze(scope)
            else:
                raise newParseError(expression, fmt"cannot assign to {assignee.kind}")

            let assign = expression.assignExpr
            case assign.kind:
            of Ident, Literal, FunctionCall, BinOp, Unary:
                assign.analyze(scope)
            else:
                raise newParseError(expression, fmt"cannot assign from {assign.kind}")

            discard expression.inferType(scope)

        else:
            discard

    if not errors.isNil():
        raise errors

proc analyze*(expression: Expression) =
    let scope = newGlobalScope()
    expression.analyze(scope)
