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

        Signed32
        Unsigned32
        Float32
        Boolean
        String

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
    "s32": Signed32,
    "u32": Unsigned32,
    "f32": Float32,
    "bool": Boolean,
    "string": String,
}

proc defaultValue*(t: Type): Option[string] =
    case t.kind:
    of Signed32, Unsigned32, Float32:
        some[string]("0")
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
