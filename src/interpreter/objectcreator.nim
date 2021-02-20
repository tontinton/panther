import strformat
import strutils

import common/types
import ir/variables
import interpreter/objects

proc toObject*(v: Variable): Object =
    case v.typ.kind:
    of Signed32:
        result = Number[int32](value: cast[int32](parseInt(v.value)))
    of Boolean:
        result = Bool(value: v.value == BOOLEAN_TRUE)
    #of Unsigned32:
    #    result = Number[uint32](value: cast[uint32](parseUInt(v.value)))
    else:
        raise newException(LibraryError, fmt"unsupported object: {v.typ.kind}")

proc toObject*(b: bool): Object =
    Bool(value: b)
