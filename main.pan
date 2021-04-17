proc _start();  // First function in the shellcode

proc fib(a: s32) -> s32 {
    if a < 2 {
        return a;
    } else {
        return fib(a - 1) + fib(a - 2);
    }
}

// The printf symbol predetermined by an address.
// In the future I would like to add a feature of searching for a symbol at runtime.
let printf = 0x402ba0 as proc(fmt: string, num1: s32, num2: s32);

proc _start() {
    let i = 1;
    while i <= 15 {
        printf("%d: %d\n", i, fib(i));
        i = i + 1;
    }
}
