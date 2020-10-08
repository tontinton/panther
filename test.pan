proc fib(n: s32) -> s32:
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

proc main() -> s32:
    let x : s32 = 2 + 5 * 8
    let y = fib(x - 30)  # auto type inference
    if x * y > 100:
        return x + y
    elif x * y < 100:
        return x - y
    else:
        return 0
