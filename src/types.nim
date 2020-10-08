type
    TypeKind* = enum
        Undetermined
        Auto
        Signed32
        Unsigned32

    Type* = ref object
        case kind*: TypeKind
        of Undetermined:
            value*: string
        else:
            discard

const BUILTIN_TYPES* = {
    "auto": Auto,
    "s32": Signed32,
    "u32": Unsigned32,
}
