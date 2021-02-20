import strutils

type
    Object = tuple[
      eq: proc(this: Object, other: Object): bool {.closure.},
      gt: proc(this: Object, other: Object): bool {.closure.},
      lt: proc(this: Object, other: Object): bool {.closure.},
      gte: proc(this: Object, other: Object): bool {.closure.},
      lte: proc(this: Object, other: Object): bool {.closure.},
      isTrue: proc(this: Object): bool {.closure.},
      add: proc(this: Object, other: Object): Object {.closure.},
      subtract: proc(this: Object, other: Object): Object {.closure.},
      multiply: proc(this: Object, other: Object): Object {.closure.},
      divide: proc(this: Object, other: Object): Object {.closure.},
      repr: proc(this: Object): string {.closure.},      
    ]

proc newNumber*[T](): Object =
    var value: T
    return (eq: proc (o: Number[T]): bool = value == o.value,
            getter1: proc (): int = result = shared1,
            getter2: proc (): int = return shared2)

# TODO: Add handling of unimplemented methods
method eq*(this: Object, other: Object): bool {.base.} = discard

method gt*(this: Object, other: Object): bool {.base.} = discard

method lt*(this: Object, other: Object): bool {.base.} =
    not gt(this, other) and not eq(this, other)

method gte*(this: Object, other: Object): bool {.base.} =
    gt(this, other) and eq(this, other)

method lte*(this: Object, other: Object): bool {.base.} =
    lt(this, other) and eq(this, other)

method isTrue*(this: Object): bool {.base.} = discard

method add*(this: Object, other: Object): Object {.base.} = discard

method subtract*(this: Object, other: Object): Object  {.base.} = discard

method multiply*(this: Object, other: Object): Object {.base.} = discard

method divide*(this: Object, other: Object): Object {.base.} = discard

method `$`*(this: Object): string {.base.} =
    "Object"

type Number*[T] = ref object of Object
    value*: T

method eq*[T](this: Number[T], other: Number[T]): bool =
    this.value == other.value

method gt*[T](this: Number[T], other: Number[T]): bool =
    this.value > other.value

method lt*[T](this: Number[T], other: Number[T]): bool =
    this.value < other.value

method gte*[T](this: Number[T], other: Number[T]): bool =
    this.value >= other.value

method lte*[T](this: Number[T], other: Number[T]): bool =
    this.value <= other.value

method isTrue*[T](this: Number[T]): bool =
    this.value > 0

method add*[T](this: Number[T], other: Number[T]): Number[T] {.base.} =
    Number[T](value: this.value + other.value)

method subtract*[T](this: Number[T], other: Number[T]): Number[T] {.base.} =
    Number[T](value: this.value - other.value)

method multiply*[T](this: Number[T], other: Number[T]): Number[T] {.base.} =
    Number[T](value: this.value * other.value)

method `$`*[T](this: Number[T]): string =
    intToStr(this.value)

type Bool* = ref object of Object
    value*: bool

method eq*(this: Bool, other: Bool): bool =
    this.value == other.value

method gt*(this: Bool, other: Bool): bool =
    this.value > other.value

method lt*(this: Bool, other: Bool): bool =
    this.value < other.value

method gte*(this: Bool, other: Bool): bool =
    this.value >= other.value

method lte*(this: Bool, other: Bool): bool =
    this.value <= other.value

method isTrue*(this: Bool): bool =
    this.value

method `$`*(this: Bool): string =
    if this.value: "true" else: "false"
