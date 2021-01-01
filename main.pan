let ITERATIONS = 25
let ITERATIONS_DOUBLED = ITERATIONS * 2

proc fib(a: s32) -> s32:
    if a <= 2:
        return 1
    return fib(a - 1) + fib(a - 2)

proc main() -> s32:
    return fib(ITERATIONS_DOUBLED - 10)
