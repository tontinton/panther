import strformat
import tables
import stacks

import ir/opcodes
import interpreter/[objects, objectcreator]

type
    Interpreter = ref object
        stack: Stack[Object]
        variables: TableRef[int, Object]
        functions: TableRef[string, ByteCode]

proc newInterpreter*(): Interpreter =
    Interpreter(stack: Stack[Object](),
                variables: newTable[int, Object](),
                functions: newTable[string, ByteCode]())

proc pop*(interpreter: Interpreter): Object =
    interpreter.stack.pop()

proc evaluate*(interpreter: Interpreter, code: ByteCode) =
    template pop(): Object =
        interpreter.stack.pop()

    template push(v: Object) =
        interpreter.stack.push(v)    

    var i = 0
    while i < code.opcodes.len():
        let opcode = code.opcodes[i]

        case opcode.kind:
        of StoreVar:
            interpreter.variables[opcode.value] = pop()

        of LoadVar:
            push(interpreter.variables[opcode.value])

        of LoadConst:
            push(code.consts[opcode.value].toObject())

        of Compare:
            let right = pop()            
            let left = pop()
            case opcode.compare:
            of Equal:
                push(left.eq(right).toObject())
            of BiggerThan:
                push(left.gt(right).toObject())
            of BiggerThanEqual:
                push(left.gte(right).toObject())
            of SmallerThan:
                push(left.lt(right).toObject())
            of SmallerThanEqual:
                push(left.lte(right).toObject())

        of JumpFalse:
            let val = pop()
            if not val.isTrue():
                i = opcode.value
                continue

        of JumpTrue:
            let val = pop()
            if val.isTrue():
                i = opcode.value
                continue

        of Add:
            let right = pop()
            let left = pop()
            push(left.add(right))

        of Subtract:
            let right = pop()
            let left = pop()
            push(left.subtract(right))

        of Multiply:
            let right = pop()
            let left = pop()
            push(left.multiply(right))

        of Divide:
            let right = pop()
            let left = pop()
            push(left.divide(right))

        of Function:
            interpreter.functions[opcode.name] = opcode.code

        of Call:
            interpreter.evaluate(interpreter.functions[opcode.call])

        of Return:
            break

        else:
            raise newException(LibraryError, fmt"unsupported eval: {opcode.kind}")

        inc(i)

proc evaluate*(code: ByteCode): Object =
    let interpreter = newInterpreter()
    interpreter.evaluate(code)
    result = interpreter.pop()
    echo result
