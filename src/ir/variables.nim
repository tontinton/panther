import strformat

import frontend/ast
import common/types

type
    Variable* = ref object
        typ*: Type  # `type` is a saved keyword in nim
        value*: string

func pretty*(v: Variable, showType: bool = true): string =
    result = v.value
    if showType:
        result &= &" ({v.typ})"

func `$`*(v: Variable): string =
    v.pretty()

func `==`*(v: Variable, other: Variable): bool =
    v.typ == other.typ and v.value == other.value

proc toVariable*(expression: Expression): Variable =
    if expression.kind != Literal:
        raise newException(LibraryError, fmt"cannot const {expression.kind} expression")

    let value = expression.literal
    let typ = expression.literalType

    return Variable(typ: typ, value: value)
