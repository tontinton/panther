import tables
import strformat
import sequtils
import options
import macros

import optionsutils

import ast
import tokens
import common/types
import common/customerrors

type
    Function = ref object
        expression: Expression
        params: seq[(string, Type)]
        returnType: Type
        implemented: bool  # To keep track of declarations vs implemented function

    Scope = ref object
        names: TableRef[string, Type]
        functions: TableRef[string, Function]
        types: TableRef[string, Type]
        insideFunction: Option[string]
        insideLoop: bool

func newFunction(expression: Expression,
                 params: seq[(string, Type)],
                 returnType: Type,
                 implemented: bool): Function =
    Function(expression: expression,
             params: params,
             returnType: returnType,
             implemented: implemented)

func newGlobalScope(): Scope =
    var types = newTable[string, Type]()
    for (key, val) in BUILTIN_TYPES:
        types[key] = Type(kind: val, ptrLevel: 0)

    Scope(names: newTable[string, Type](),
          functions: newTable[string, Function](),
          types: types,
          insideFunction: none[string](),
          insideLoop: false)

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

    Scope(names: names, functions: functions, types: types, insideFunction: some(funcName), insideLoop: false)

proc defaultExpression(typ: Type, token: Token): Expression =
    withSome typ.defaultValue():
        some val:
            Expression(kind: Literal, literalType: typ, literal: val, token: token)
        none:
            Expression(kind: Empty)

proc isUndetermined(t: Type): bool =
    case t.kind:
    of Undetermined, UndeterminedProcedure:
        true
    else:
        false

proc inferType(expression: Expression, scope: Scope): Type =
    proc validateType(t: Type, name: string) =
        if t.isUndetermined():
            raise newParseError(expression, fmt"`{name}` wasn't patched to a valid type")

        case t.kind:
        of Auto:
            raise newParseError(expression, fmt"could not infer from a non inferred {t.kind} `{name}`")
        else:
            discard

    case expression.kind:
    of Empty:
        return Type(kind: Void)

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
            raise newParseError(expression, fmt"the function `{name}` wasn't declared yet")

    of Assign:
        let leftType = expression.assignee.inferType(scope)
        let rightType = expression.assignExpr.inferType(scope)
        if leftType != rightType:
            raise newParseError(expression,
                                fmt"types differ on {expression.token.kind} operation, {leftType} != {rightType}")
        return leftType

    of Cast:
        return expression.toType

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

    func validateUnique(scope: Scope, name: string, checkFunctions: bool = true) =
        if name in scope.names:
            raise newParseError(expression, fmt"`{name}` is already declared as a variable")
        if checkFunctions and name in scope.functions:
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
        # No need to validate uniqueness, as we expect the caller to have validated already
        scope.functions[key] = val

    proc fixedUndeterminedType(undetermined: Type): Type =
        case undetermined.kind:
        of Undetermined:
            let typeName = undetermined.value
            let ptrLevel = undetermined.ptrLevel
            try:
                return Type(kind: scope.types[typeName].kind, ptrLevel: ptrLevel)
            except KeyError:
                raise newParseError(expression, fmt"the type `{typeName}` wasn't declared yet")
        of UndeterminedProcedure:
            var params: seq[(string, Type)] = @[]
            for (name, typ) in undetermined.params:
                params.add((name, typ.fixedUndeterminedType()))
            return Type(kind: Procedure, params: params, ret: undetermined.ret.fixedUndeterminedType())
        else:
            # The type is already determined, just return it
            return undetermined

    withErrorCatching:
        case expression.kind:
        of Block:
            let funcCount = scope.functions.len()
            var returnFound = false

            # Filter out all empty expressions from the block
            expression.expressions = filter(expression.expressions, proc(e: Expression): bool = e.kind != Empty)

            for exp in expression.expressions:
                withErrorCatching:
                    if returnFound:
                        # TODO: Show the return expression as well in the error message
                        # TODO: Show all expression after return in a single error message 
                        raise newParseError(exp, fmt"cannot have an expression after {Return}")
                    if exp.kind == Return:
                        returnFound = true

                    exp.analyze(scope)

            if scope.functions.len() > funcCount:
                # Functions were added, check that they are all implemented.
                # Currently, checking that ALL functions are implemented not only the ones added.
                for (name, f) in scope.functions.pairs():
                    withErrorCatching:
                        if not f.implemented and not f.expression.extern:
                            raise newParseError(f.expression, fmt"`{name}` was declared, but never implemented")

        of FunctionDeclaration:
            expression.declParams.validateKind(Block)

            let name = expression.declName

            case expression.implementation.kind:
            of Block:
                if expression.extern:
                    # TODO: warning instead?
                    raise newParseError(expression, "Implemented an extern function?")
            of Empty:
                discard
            else:
                let msg = fmt"expected nothing or an implementation block for {name}, not {expression.implementation.kind}"
                raise newParseError(expression, msg)

            let declaredFunc = scope.functions.getOrDefault(name, nil)
            if declaredFunc != nil:
                if declaredFunc.implemented:
                    raise newParseError(expression, fmt"`{name}` is already declared as a function")
                elif expression.implementation.kind != Block:
                    raise newParseError(expression, fmt"expected `{name}` to have an implementation")

            # No need to check function uniqueness, we just did that ourselves
            scope.validateUnique(name, checkFunctions=false)

            if expression.returnType.isUndetermined():
                expression.returnType = expression.returnType.fixedUndeterminedType()

            let functionScope = newFunctionScope(scope, name)
            var paramsDescription = newSeq[(string, Type)]()

            for exp in expression.declParams.expressions:
                case exp.kind:
                of TypedIdent:
                    exp.analyze(scope)
                    functionScope.add(exp.ident.value, exp.identType)
                    paramsDescription.add((exp.ident.value, exp.identType))
                of Ident:
                    raise newParseError(expression, fmt"`{exp.value}` has no type")
                of Assign:
                    raise newParseError(expression, "default parameters are currently unsupported")
                else:
                    raise newParseError(expression, fmt"{exp[]} is an invalid function parameter")

            let funcDescription = newFunction(expression,
                                              paramsDescription,
                                              expression.returnType,
                                              expression.implementation.kind == Block)
            scope.add(name, funcDescription)

            if expression.implementation.kind == Block:
                functionScope.add(name, funcDescription)
                expression.implementation.analyze(functionScope)

                # Add return expression if needed at the end of the function
                let expressions = expression.implementation.expressions
                if expressions.len() == 0 or expressions[expressions.len() - 1].kind != Return:
                    let whatToReturn = expression.returnType.defaultExpression(expression.token)
                    let returnExpression = Expression(kind: Return,
                                                      retExpr: whatToReturn,
                                                      token: expression.token)
                    expression.implementation.expressions.add(returnExpression)

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

            for (paramExpr, param) in zip(paramExpressions, function.params):
                let (_, paramType) = param
                if not paramExpr.isResultExpression():
                    raise newParseError(expression, fmt"{paramExpr[]} is an invalid function call parameter")

                paramExpr.analyze(scope)

                let inferredType = paramExpr.inferType(scope)
                if paramType != inferredType:
                    raise newParseError(expression,
                                        fmt"types differ on function call `{name}`, {paramType} != {inferredType}")

        of Declaration:
            let declExpr = expression.declExpr

            case declExpr.kind:
            of Assign:
                let left = declExpr.assignee
                let right = declExpr.assignExpr

                let (assigneeName, assigneeType) = case left.kind:
                of Ident:
                    right.analyze(scope)
                    let identType = right.inferType(scope)
                    scope.add(left.value, identType)
                    declExpr.assignee = Expression(kind: TypedIdent,
                                                   identType: identType,
                                                   ident: left,
                                                   token: left.token)
                    (left.value, identType)
                of TypedIdent:
                    left.analyze(scope)
                    scope.add(left.ident.value, left.identType)
                    (left.ident.value, left.identType)
                else:
                    raise newParseError(expression, fmt"cannot assign to {expression.assignee.kind}")

                if assigneeType.kind == Procedure:
                    let funcDescription = newFunction(expression,
                                                      assigneeType.params,
                                                      assigneeType.ret,
                                                      true)
                    scope.add(assigneeName, funcDescription)

                declExpr.analyze(scope)
            of Ident, TypedIdent:
                raise newParseError(expression, "variable declaration without assignment is currently unsupported")
            else:
                raise newParseError(expression, "invalid declaration of a variable")

        of TypedIdent:
            expression.ident.validateKind(Ident)
            if expression.identType.isUndetermined():
                expression.identType = expression.identType.fixedUndeterminedType()

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
                if not right.isResultExpression():
                    raise newParseError(expression, fmt"invalid right side of {leftKind}")

                right.analyze(scope)

        of BinOp:
            if not expression.left.isResultExpression():
                raise newParseError(expression, fmt"invalid left side of {expression.token.kind}")

            if not expression.right.isResultExpression():
                raise newParseError(expression, fmt"invalid right side of {expression.token.kind}")

            expression.left.analyze(scope)
            expression.right.analyze(scope)
            discard expression.inferType(scope)

        of IfThen:
            withErrorCatching:
                if not expression.condition.isResultExpression():
                    raise newParseError(expression, fmt"invalid condition expression")

                if expression.then.kind != Block:
                    raise newParseError(expression, fmt"got {expression.then.kind}, but expected {Block}")

                expression.condition.analyze(scope)

            let prevInsideLoop = scope.insideLoop
            if expression.token.kind == While:
                scope.insideLoop = true

            expression.then.analyze(scope)

            scope.insideLoop = prevInsideLoop

        of IfElseThen:
            expression.ifThen.analyze(scope)
            expression.otherwise.analyze(scope)

        of Breakage:
            if not scope.insideLoop:
                raise newParseError(expression, fmt"cannot {expression.token.kind} ouside of a loop")

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
            if not assign.isResultExpression():
                raise newParseError(expression, fmt"cannot assign from {assign.kind}")

            assign.analyze(scope)
            discard expression.inferType(scope)

        of Cast:
            if expression.toType.isUndetermined():
                expression.toType = expression.toType.fixedUndeterminedType()

            let exp = expression.castExpr
            if not exp.isResultExpression():
                raise newParseError(expression, fmt"left side must be castable")

            exp.analyze(scope)

        else:
            discard

    if not errors.isNil():
        raise errors

proc analyze*(expression: Expression) =
    let scope = newGlobalScope()
    expression.analyze(scope)
