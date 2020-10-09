## Pure Nim stack implementation based on sequences.
##
## **Example:**
##
## .. code-block:: Nim
##   # Reverting a string using a stack
##
##   import stacks
##
##   let a = "Hello, World!"
##   var s = Stack[char]()
##   for letter in a:
##       s.push(letter)
##
##   var b: string
##   while not s.isEmpty:
##       b.add(s.pop)
##
##   assert b == "!dlroW ,olleH"
##

import math

type EStackEmpty* = object of CatchableError

type
    Stack* [T] = object
        data: seq[T]

proc newStack* [T](capacity = 8): Stack[T] =
    ## Creates a new stack.
    ## Optionally, the initial capacity can be reserved via `capacity` parameter.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int](capacity = 64)
    assert isPowerOfTwo(capacity)
    result.data = newSeqOfCap[T](capacity)

proc len* [T](s: Stack[T]): int =
    ## Returns the number of elements in the stack.
    ## Returns `0` if the stack is empty.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   assert a.len == 0
    ##   a.push(10); a.push(20)
    ##   assert a.len == 2
    s.data.len()

proc empty* [T](s: Stack[T]): bool {.deprecated: "use isEmpty() instead".} =
    s.data.len() == 0

proc isEmpty* [T](s: Stack[T]): bool =
    ## Returns `true` if stack contains no elements, `false` otherwise.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   assert a.isEmpty == true
    ##   a.push(10)
    ##   assert a.isEmpty == false
    s.data.len() == 0

proc push* [T](s: var Stack[T], element: T) =
    ## Pushes `element` onto the top of the stack.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   a.push(10)
    s.data.add(element)

proc pop* [T](s: var Stack[T]): T {.raises: [EStackEmpty].} =
    ## Pops the top element from the stack.
    ## Raises `EStackEmpty` exception if the stack is empty.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   a.push(10)
    ##   discard a.pop()
    ##   doAssertRaises(EStackEmpty, echo a.pop())
    if not s.isEmpty:
        result = s.data[^1]
        s.data.setLen s.data.len - 1
    else:
        raise newException(EStackEmpty, "Cannot pop an empty stack")

proc popUnsafe* [T](s: var Stack[T]): T =
    ## Pops the top element from the stack without checking if it's not empty.
    ## Make sure the stack is not empty.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   a.push(10)
    ##   check(a.popUnsafe() == 10)
    result = s.data[^1]
    s.data.setLen s.data.len - 1

proc peek* [T](s: Stack[T]): T {.raises: [EStackEmpty].} =
    ## Peeks the top element from the stack.
    ## Raises `EStackEmpty` exception if the stack is empty.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   a.push(10)
    ##   check(a.peek() == 10)
    if not s.isEmpty:
        result = s.data[^1]
    else:
        raise newException(EStackEmpty, "Cannot peek an empty stack")

proc peekUnsafe* [T](s: Stack[T]): T =
    ## Peeks the top element from the stack without checking if it's not empty.
    ## Make sure the stack is not empty.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   a.push(10)
    ##   check(a.peekUnsafe() == 10)
    s.data[^1]

proc clear* [T](s: var Stack[T]) =
    ## Empties the stack. Does nothing if the stack is already empty.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   a.push(10)
    ##   a.clear()
    ##   assert a.isEmpty == true
    if not s.isEmpty:
        s.data.setLen 0

proc toSeq* [T](s: Stack[T]): seq[T] =
    ## Returns sequence representation of a stack.
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   a.push(10); a.push(20)
    ##   assert a.toSeq() == @[10, 20]
    s.data

proc `$`* [T](s: Stack[T]): string =
    ## Returns string representation of a stack
    ##
    ## .. code-block:: Nim
    ##   var a = newStack[int]()
    ##   a.push(10); a.push(20)
    ##   assert $a == "Stack[10, 20]"
    result = "Stack["
    if not s.isEmpty():
        for i in 0 .. s.data.high() - 1:
            result &= $s.data[i]
            result &= ", "
        result &= $s.data[^1]
    result &= "]"
