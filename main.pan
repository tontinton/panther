let TEST_NUMBER = 30

proc _start()

proc fib(a: s32) -> s32:
    if a < 2:
        return a
    else:
        return fib(a - 1) + fib(a - 2)

let printf = 0x402ba0 as proc(fmt: string, num: s32) 

proc _start():
    printf("%d\n", fib(TEST_NUMBER))
