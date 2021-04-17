<img src="./logo.svg">

## Introduction
The panther language is a language made for exploitations.

It's main focus is **minimal code size** through code size 
optimizations and compiling directly to a **shellcode** (position independent code).

## Flow

1. **Lexer**: Parse text file into tokens
2. **Parser**: Parse tokens into AST
3. **Analyzer**: Validate that the AST is correct panther syntax
4. **Bytecode**: Parse AST into bytecode
5. **LLVM**: Parse bytecode into LLVM IR
6. **Optimizations + Backend**: llvm

## What can it do currently? 
Currently, the panther binary can compile a single file using the llvm backend, with minimal type inference and type checking.

When running the panther compiler on ``main.pan``
```bash
# Compile the panther compiler
nimble build -d:release

# Panther compiles an object file, in this example I compile to a windows COFF
./panther c -i main.pan -o output.o -t x86_64-pe-windows-coff

# Link the object file to an executable, needed to get rid of relocations
ld -nostartfiles output.o -o output.exe

# Extract the .shell section created by the compiler
objcopy -j .shell -O binary output.exe shellcode.bin
```

When ``main.pan`` looks like:
```rust
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
let printf = 0x402ba0 as proc(fmt: string, num: s32);

proc _start() {
    let i = 1;
    while i <= 15 {
        printf("%d\n", fib(i));
        i = i + 1;
    }
}
```

Results in a shellcode (``shellcode.bin``) that prints the the first 15 fibonnaci numbers.
