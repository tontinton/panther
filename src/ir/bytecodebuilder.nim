import tables
import strformat

import opcodes
import variables
import frontend/ast
import frontend/tokens
import common/types

const FIX_INDEX = -1

type
    ByteCodeBuilder = ref object
        code: ByteCode

        # Map between name of a variable to it's index inside variables
        nameToVariable: TableRef[string, int]

        # To be able to insert jumps instead of and / or opcodes,
        # we need to update the jump values of all jumps inside a
        # bin op tree after it's done feeding opcodes.
        inBinOpTree: bool

proc newByteCodeBuilder*(builder: ByteCodeBuilder = nil): ByteCodeBuilder =
    let variableTable = newTable[string, int]()

    if builder != nil:
        for key in builder.nameToVariable.keys():
            variableTable[key] = builder.nameToVariable[key]

    ByteCodeBuilder(code: newByteCode(),
                    nameToVariable: variableTable,
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

        let funcBuilder = newByteCodeBuilder(builder=builder)

        var arguments = newSeq[Type]()
        for exp in expression.declParams.expressions:
            arguments.add(exp.identType)
            discard funcBuilder.storeVar(exp.ident.value)

        funcBuilder.feed(expression.implementation)

        add(Opcode(kind: Function, name: name, arguments: arguments, ret: ret, code: funcBuilder.code))

    of FunctionCall:
        for param in expression.params.expressions:
            builder.feed(param)
        add(Opcode(kind: Call, call: expression.name))

    of ast.Return:
        builder.feed(expression.retExpr)
        add(Opcode(kind: opcodes.Return))

    of Declaration:
        builder.feed(expression.declExpr)

    of Assign:
        var left = expression.assignee

        builder.feed(expression.assignExpr)

        case left.kind:
        of TypedIdent:
            # ignore type of typed ident
            add(builder.storeVar(left.ident.value))            
        of Unary:
            builder.feed(left)
            add(Opcode(kind: Store))
        else:
            raise newException(LibraryError, fmt"unsupported assign: {expression.token.kind}")            

    of Unary:
        case expression.token.kind:
        of Mul:
            builder.feed(expression.unaryExpr)
            add(Opcode(kind: Load))
        else:
            raise newException(LibraryError, fmt"unsupported unary: {expression.token.kind}")            

    of Literal:
        add(builder.loadConst(expression.toVariable()))

    of Ident:
        add(builder.loadVar(expression.value))

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
        let afterThen = builder.code.opcodes.len()

        for i in start..<afterCond:
            case builder.code.opcodes[i].kind:
            of JumpTrue:
                inc(builder.code.opcodes[i].value)
            of JumpFalse:
                builder.code.opcodes[i].value = afterThen
            else:
                discard

    of IfElseThen:
        builder.feed(expression.ifThen)
        builder.feed(expression.otherwise)

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
