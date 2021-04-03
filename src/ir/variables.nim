import strformat

import frontend/ast
import common/types

const MAX_VALUE_DISPLAY_LENGTH = 20

type
    Variable* = ref object
        typ*: Type  # `type` is a saved keyword in nim
        value*: string

func pretty*(v: Variable, showType: bool = true): string =
    result = ""

    let value = if v.value.len() > MAX_VALUE_DISPLAY_LENGTH:
        v.value[0..<MAX_VALUE_DISPLAY_LENGTH] & "..."
    else:
        v.value

    if v.typ.kind == String:
        result.addQuoted(value)
    else:
        result = value

    if showType:
        result &= &" ({v.typ})"

func `$`*(v: Variable): string =
    v.pretty()

proc toVariable*(expression: Expression): Variable =
    if expression.kind != Literal:
        raise newException(LibraryError, fmt"cannot const {expression.kind} expression")

    let value = expression.literal
    let typ = expression.literalType

    return Variable(typ: typ, value: value)
