import tables
import strformat

import opcodes
import variables
import frontend/ast
import frontend/tokens
import common/types

const FIX_INDEX = -1
const FIX_TO_START = -2
const FIX_TO_END = -3

type
    ByteCodeBuilder = ref object
        code: ByteCode

        # Map between name of a variable to it's index inside variables
        nameToVariable: TableRef[string, int]

        # List of all defined functions,
        # used for knowing if to load the function as a pointer,
        # when handling a FunctionCall expression.
        definedFunctions: seq[string]

        # To be able to insert jumps instead of and / or opcodes,
        # we need to update the jump values of all jumps inside a
        # bin op tree after it's done feeding opcodes.
        inBinOpTree: bool

proc newByteCodeBuilder*(builder: ByteCodeBuilder = nil): ByteCodeBuilder =
    let variableTable = newTable[string, int]()
    var definedFunctions : seq[string] = @[]

    if builder != nil:
        for key in builder.nameToVariable.keys():
            variableTable[key] = builder.nameToVariable[key]
        for val in builder.definedFunctions:
            definedFunctions.add(val)

    ByteCodeBuilder(code: newByteCode(),
                    nameToVariable: variableTable,
                    definedFunctions: definedFunctions,
                    inBinOpTree: false)

proc loadConst(builder: ByteCodeBuilder, variable: Variable): Opcode =
    let index = builder.code.consts.len()
    builder.code.consts.add(variable)
    return Opcode(kind: LoadConst, value: index)

proc loadVar(builder: ByteCodeBuilder, name: string): Opcode =
    return Opcode(kind: LoadVar, value: builder.nameToVariable[name])

proc storeVar(builder: ByteCodeBuilder, name: string): Opcode =
    var index = builder.nameToVariable.getOrDefault(name, -1)
    if -1 == index:
        index = builder.nameToVariable.len()
        builder.nameToVariable[name] = index
    return Opcode(kind: StoreVar, value: index)

proc feed(builder: ByteCodeBuilder, expression: Expression) =
    template add(opcode: Opcode) =
        builder.code.opcodes.add(opcode)

    case expression.kind:
    of Block:
        for e in expression.expressions:
            builder.feed(e)

    of FunctionDeclaration:
        let name = expression.declName
        let ret = expression.returnType

        let index = builder.storeVar(name).value
        builder.definedFunctions.add(name)

        let funcBuilder = newByteCodeBuilder(builder=builder)

        var params = newSeq[(string, Type)]()
        for exp in expression.declParams.expressions:
            params.add((exp.ident.value, exp.identType))
            discard funcBuilder.storeVar(exp.ident.value)

        funcBuilder.feed(expression.implementation)

        add(Opcode(kind: Function,
                   name: name,
                   index: index,
                   funcType: Type(kind: Procedure, params: params, ret: ret),
                   code: funcBuilder.code))

    of FunctionCall:
        for param in expression.params.expressions:
            builder.feed(param)
        add(builder.loadVar(expression.name))

        if not (expression.name in builder.definedFunctions):
            # The call is to a function pointer and not a declaration, load the pointer
            add(Opcode(kind: Load))

        add(Opcode(kind: Call))

    of ast.Return:
        if expression.retExpr.kind != Empty:
            builder.feed(expression.retExpr)
            add(Opcode(kind: opcodes.Return))
        else:
            add(Opcode(kind: opcodes.ReturnNothing))

    of Declaration:
        builder.feed(expression.declExpr)

    of Assign:
        var left = expression.assignee

        builder.feed(expression.assignExpr)

        case left.kind:
        of Ident:
            add(builder.storeVar(left.value))
        of TypedIdent:
            # ignore type of typed ident
            add(builder.storeVar(left.ident.value))
        of Unary:
            builder.feed(left.unaryExpr)
            add(Opcode(kind: Store))
        else:
            raise newException(LibraryError, fmt"unsupported assign: {left.kind}")

    of Unary:
        case expression.token.kind:
        of Mul:
            builder.feed(expression.unaryExpr)
            add(Opcode(kind: Load))
        of tokens.Not:
            builder.feed(expression.unaryExpr)
            add(Opcode(kind: opcodes.Not))
        of Ampersand:
            add(builder.loadVar(expression.unaryExpr.value))
        else:
            raise newException(LibraryError, fmt"unsupported unary: {expression.token.kind}")

    of Literal:
        add(builder.loadConst(expression.toVariable()))

    of Ident:
        add(builder.loadVar(expression.value))
        add(Opcode(kind: Load))

    of ast.Cast:
        builder.feed(expression.castExpr)
        add(Opcode(kind: opcodes.Cast, toType: expression.toType))

    of BinOp:
        let root = not builder.inBinOpTree
        let start = if root:
            builder.inBinOpTree = true
            builder.code.opcodes.len()
        else:
            -1

        case expression.token.kind:
        of tokens.And, tokens.Or:
            builder.feed(expression.left)

            if expression.token.kind == tokens.And:
                add(Opcode(kind: JumpFalse, value: FIX_INDEX))
            else:
                add(Opcode(kind: JumpTrue, value: FIX_INDEX))

            builder.feed(expression.right)

        else:
            builder.feed(expression.left)
            builder.feed(expression.right)

            case expression.token.kind:
            of tokens.Plus:
                add(Opcode(kind: Add))
            of tokens.Minus:
                add(Opcode(kind: Subtract))
            of tokens.Mul:
                add(Opcode(kind: Multiply))
            of tokens.Div:
                add(Opcode(kind: Divide))
            of tokens.BiggerThan:
                add(Opcode(kind: Compare, compare: opcodes.BiggerThan))
            of tokens.BiggerThanEqual:
                add(Opcode(kind: Compare, compare: opcodes.BiggerThanEqual))
            of tokens.SmallerThan:
                add(Opcode(kind: Compare, compare: opcodes.SmallerThan))
            of tokens.SmallerThanEqual:
                add(Opcode(kind: Compare, compare: opcodes.SmallerThanEqual))
            of tokens.DoubleEqual:
                add(Opcode(kind: Compare, compare: opcodes.Equal))
            else:
                raise newException(LibraryError, fmt"unsupported binop: {expression.token.kind}")

        if root:
            let afterRoot = builder.code.opcodes.len()
            for i in start..<afterRoot:
                case builder.code.opcodes[i].kind:
                of JumpTrue, JumpFalse:
                    if builder.code.opcodes[i].value == FIX_INDEX:
                        builder.code.opcodes[i].value = afterRoot
                else:
                    discard

            builder.inBinOpTree = false

    of IfThen:
        let start = builder.code.opcodes.len()

        builder.feed(expression.condition)

        add(Opcode(kind: JumpFalse, value: -1))
        let afterCond = builder.code.opcodes.len()

        builder.feed(expression.then)

        if expression.token.kind == While:
            add(Opcode(kind: Jump, value: start))

        let afterThen = builder.code.opcodes.len()

        for i in start..<afterCond:
            case builder.code.opcodes[i].kind:
            of JumpTrue:
                inc(builder.code.opcodes[i].value)
            of JumpFalse:
                builder.code.opcodes[i].value = afterThen
            else:
                discard

        if expression.token.kind == While:
            for i in afterCond..<afterThen:
                case builder.code.opcodes[i].kind:
                of Jump:
                    case builder.code.opcodes[i].value:
                    of FIX_TO_START:
                        builder.code.opcodes[i].value = start
                    of FIX_TO_END:
                        builder.code.opcodes[i].value = afterThen
                    else:
                        discard
                else:
                    discard

    of IfElseThen:
        let beforeIf = builder.code.opcodes.len()

        builder.feed(expression.ifThen)

        let lastOpcode = builder.code.opcodes[builder.code.opcodes.len() - 1]
        let flowBreak = lastOpcode.kind == opcodes.Return

        if flowBreak:
            # The flow already breaks, no need to enter a jump opcode
            builder.feed(expression.otherwise)
        else:
            var jmp = Opcode(kind: Jump, value: -1)
            add(jmp)
            let afterJump = builder.code.opcodes.len()

            builder.feed(expression.otherwise)
            let afterElse = builder.code.opcodes.len()

            jmp.value = afterElse

            # TODO: fix this mess, everytime I add a jump instruction it destroys the whole flow
            for i in beforeIf..<afterJump:
                case builder.code.opcodes[i].kind:
                of JumpTrue:
                    inc(builder.code.opcodes[i].value)
                of JumpFalse:
                    builder.code.opcodes[i].value = afterJump
                else:
                    discard

    of Breakage:
        case expression.token.kind:
        of Continue:
            add(Opcode(kind: Jump, value: FIX_TO_START))
        of Break:
            add(Opcode(kind: Jump, value: FIX_TO_END))
        else:
            raise newException(LibraryError, fmt"unsupported breakage: {expression.token.kind}")

    of Empty:
        discard

    else:
        # TODO: better errors
        raise newException(LibraryError, fmt"cannot turn to bytecode: {expression.kind}")

proc build*(builder: ByteCodeBuilder, expression: Expression): ByteCode =
    builder.feed(expression)
    result = builder.code
    builder.code = newByteCode()

proc byteCode*(expression: Expression): ByteCode =
    let builder = newByteCodeBuilder()
    builder.feed(expression)
    builder.code
