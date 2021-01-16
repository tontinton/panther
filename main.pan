let ITERATIONS = 25
let ITERATIONS_DOUBLED = ITERATIONS * 2

proc fib(a: s32) -> s32:
    if a <= 2:
        return 1
    return fib(a - 1) + fib(a - 2)

proc main() -> s32:
    let test = &ITERATIONS_DOUBLED
    let test_ptr = &test
    **test_ptr = 10
    return fib(*test)
