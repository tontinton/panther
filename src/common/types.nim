import strutils
import strformat

import safeoptions


type
    TypeKind* = enum
        Undetermined
        UndeterminedProcedure  # Same as Procedure, but it's types are undetermined

        Auto
        Void
        Procedure

        Boolean
        String

        # When adding integer types, remember to update the is*Integer functions
        Signed8
        Signed16
        Signed32
        Signed64

        Unsigned8
        Unsigned16
        Unsigned32
        Unsigned64

        Float16
        Float32
        Float64

    Type* = ref object
        ptrLevel*: int  # type* == 1, type*** == 3
        case kind*: TypeKind
        of Undetermined:
            value*: string
        of UndeterminedProcedure, Procedure:
            params*: seq[(string, Type)]  # (name, type)
            ret*: Type
        else:
            discard

const BOOLEAN_TRUE* = "true"
const BOOLEAN_FALSE* = "false"

const BUILTIN_TYPES* = {
    "auto": Auto,
    "void": Void,

    "bool": Boolean,
    "string": String,

    "s8": Signed8,
    "s16": Signed16,
    "s32": Signed32,
    "s64": Signed64,

    "u8": Unsigned8,
    "u16": Unsigned16,
    "u32": Unsigned32,
    "u64": Unsigned64,

    "f16": Float16,
    "f32": Float32,
    "f64": Float64,
}

proc isSignedInteger*(t: Type): bool =
    if t.ptrLevel > 0:
        return false

    case t.kind:
    of Signed8, Signed16, Signed32, Signed64:
        true
    else:
        false

proc isUnsignedInteger*(t: Type): bool =
    if t.ptrLevel > 0:
        return true

    case t.kind:
    of Unsigned8, Unsigned16, Unsigned32, Unsigned64:
        true
    else:
        false

proc isRealInteger*(t: Type): bool =
    if t.ptrLevel > 0:
        return false

    case t.kind:
    of Float16, Float32, Float64:
        true
    else:
        false

proc isInteger*(t: Type): bool =
    isSignedInteger(t) or isUnsignedInteger(t) or isRealInteger(t)

proc defaultValue*(t: Type): Option[string] =
    if t.isInteger():
        return some[string]("0")

    case t.kind:
    of String:
        some[string]("")
    of Boolean:
        some[string](BOOLEAN_FALSE)
    else:
        none[string]()

proc `$`*(t: Type): string =
    case t.kind:
    of Procedure:
        var params = ""
        for i, (_, typ) in t.params:
            params &= $typ
            if i != t.params.len() - 1:
                params &= ", "
        &"<({params}) -> {$t.ret}>"
    else:
        $(t.kind) & "*".repeat(t.ptrLevel)

proc `==`*(t1, t2: Type): bool =
    if t1.kind != t2.kind or t1.ptrLevel != t2.ptrLevel:
        return false

    case t1.kind:
    of Undetermined:
        t1.value == t2.value
    of UndeterminedProcedure, Procedure:
        t1.ret == t2.ret and t1.params == t2.params
    else:
        true

proc reference*(t: Type): Type =
    Type(kind: t.kind, ptrLevel: t.ptrLevel + 1)

proc dereference*(t: Type): Type =
    Type(kind: t.kind, ptrLevel: t.ptrLevel - 1)
