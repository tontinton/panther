import strformat
import strutils

import common/types
import variables

type
    OpcodeKind* = enum
        StoreVar
        LoadVar
        LoadConst

        Store    
        Load        

        Compare
        JumpFalse
        JumpTrue
        Jump

        Not

        Add
        Subtract
        Multiply
        Divide

        Function
        Call
        Return
        ReturnNothing

        Cast

    CompareKind* = enum
        Equal
        BiggerThan
        BiggerThanEqual
        SmallerThan
        SmallerThanEqual

    Opcode* = ref object
        case kind*: OpcodeKind
        of StoreVar, LoadVar, JumpFalse, JumpTrue, Jump, LoadConst:
            value*: int
        of Function:
            name*: string
            index*: int
            funcType*: Type
            code*: ByteCode
        of Compare:
            compare*: CompareKind
        of Cast:
            toType*: Type
        else:
            discard

    ByteCode* = ref object
        consts*: seq[Variable]
        opcodes*: seq[Opcode]

proc newByteCode*(): ByteCode =
    ByteCode(consts: @[], opcodes: @[])

func tabToString(tabs: uint): string =
    "\t".repeat(tabs)

func pretty(code: ByteCode, tabs: uint = 0): string

func pretty(opcode: Opcode, tabs: uint = 0, code: ByteCode = nil): string =
    case opcode.kind:
    of StoreVar, LoadVar, JumpFalse, JumpTrue, Jump:
        result = &"{opcode.kind} {opcode.value}"

    of LoadConst:
        result = &"{opcode.kind} {opcode.value}"
        if code != nil:
            result &= &" ({code.consts[opcode.value].pretty(showType=false)})"

    of Function:
        result = &"{opcode.kind} <{opcode.name}{opcode.funcType}> ({opcode.index})"
        let implementation = opcode.code.pretty(tabs + 1)
        if implementation != "":
            result &= &":\n{implementation}"

    of Compare:
        let c = case opcode.compare:
                of BiggerThan: ">"
                of BiggerThanEqual: ">="
                of SmallerThan: "<"
                of SmallerThanEqual: "<="
                of Equal: "=="

        result = &"{opcode.kind} {c}"

    of Cast:
        result = &"{opcode.kind} {opcode.toType}"

    else:
        result = $opcode.kind

func pretty(code: ByteCode, tabs: uint = 0): string =
    result = ""

    if code.consts.len() > 0:
        result &= &"{tabs.tabToString()}Consts:\n"
        for i, c in code.consts:
            result &= &"{(tabs + 1).tabToString()}{i}:{1.tabToString()}{c}\n"

    if code.opcodes.len() > 0:
        result &= &"{tabs.tabToString()}Opcodes:\n"
        for i, o in code.opcodes:
            result &= &"{(tabs + 1).tabToString()}{i}:{1.tabToString()}{o.pretty(tabs + 1, code=code)}\n"

func `$`*(opcode: Opcode): string =
    opcode.pretty()

func `$`*(code: ByteCode): string =
    code.pretty()
