let ITERATIONS = 25
let ADDRESS = 0x10000

proc fib(a: s32) -> s32

proc main() -> s32:
    let test = ADDRESS as s32*
    *test = ITERATIONS
    return fib(*test)

proc fib(a: s32) -> s32:
    if a <= 2:
        return 1
    return fib(a - 1) + fib(a - 2)
