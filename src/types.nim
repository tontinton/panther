import tables

type
    TypeKind* = enum
        Undetermined
        Auto
        Signed32

    Type* = ref object
        case kind*: TypeKind
        of Undetermined:
            value*: string
        else:
            discard

const TYPES = {
    "auto": Auto,
    "s32": Signed32,
}.toTable
