let TEST_NUMBER = 30

proc _start()

proc fib(a: s32) -> s32:
    if a < 2:
        return a
    else:
        return fib(a - 1) + fib(a - 2)

let printf = 0x402ba0 as proc(fmt: s8*, num: s32) 

proc print_s32(a: s32):
    let o = 0x000a6425
    printf(&o as s8*, a)

proc _start():
    print_s32(fib(TEST_NUMBER))
