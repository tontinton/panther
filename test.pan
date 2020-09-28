proc fib(n: u32) -> u32:
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

proc main() -> void:
    let x : u32 = 2 + 5 * 8
    let y = fib(x - 30)
    if x * y > 100:
        return x + y
    elif x * y < 100:
        return x - y
    else:
        return 0
