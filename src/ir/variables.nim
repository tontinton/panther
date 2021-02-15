import strformat

import frontend/ast
import common/types

type
    Variable* = ref object
        typ*: Type  # `type` is a saved keyword in nim
        value*: string

func `$`*(c: Variable): string =
    &"{c.value} ({c.typ})"

proc toVariable*(expression: Expression): Variable =
    if expression.kind != Literal:
        raise newException(LibraryError, fmt"cannot const {expression.kind} expression")

    let value = expression.literal
    let typ = expression.literalType

    return Variable(typ: typ, value: value)
