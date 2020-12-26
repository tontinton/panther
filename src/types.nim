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
