import strutils


type
    TypeKind* = enum
        Undetermined
        Auto
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
        else:
            discard

const BOOLEAN_TRUE* = "true"
const BOOLEAN_FALSE* = "false"

const BUILTIN_TYPES* = {
    "auto": Auto,
    "s32": Signed32,
    "u32": Unsigned32,
    "f32": Float32,
    "bool": Boolean,
    "string": String,
}

proc `$`*(t: Type): string =
    $(t.kind) & "*".repeat(t.ptrLevel)

proc `==`*(t1, t2: Type): bool =
    t1.kind == t2.kind and t1.ptrLevel == t2.ptrLevel

proc reference*(t: Type): Type =
    Type(kind: t.kind, ptrLevel: t.ptrLevel + 1)
