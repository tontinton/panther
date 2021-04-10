import unittest
import options

import frontend/frontend
import ir/[bytecodebuilder, opcodes, variables]

suite "bytecode builder":
    test "sanity":
        let outputAst = "let a = 5 + 9 * 2;".parseText()
        assert outputAst.isSome()
        let b = outputAst.get().byteCode()
        assert b.consts.len() == 3
        assert b.opcodes.len() == 6

        for i, expectedConstValue in @["5", "9", "2"]:
            assert b.consts[i].value == expectedConstValue

        assert b.opcodes[0].kind == LoadConst
        assert b.opcodes[0].value == 0

        assert b.opcodes[1].kind == LoadConst
        assert b.opcodes[1].value == 1

        assert b.opcodes[2].kind == LoadConst
        assert b.opcodes[2].value == 2

        assert b.opcodes[3].kind == Multiply

        assert b.opcodes[4].kind == Add

        assert b.opcodes[5].kind == StoreVar
        assert b.opcodes[5].value == 0
