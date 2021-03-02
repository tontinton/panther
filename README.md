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

When running the panther compiler on main.pan
```bash
nimble build -d:release
./panther c -i main.pan
```

When ``main.pan`` looks like:
```nim
let ITERATIONS = 25
let ADDRESS = 0x10000

proc fib(a: s32) -> s32:
    if a <= 2:
        return 1
    return fib(a - 1) + fib(a - 2)

proc main() -> s32:
    let test = ADDRESS as s32*
    *test = ITERATIONS
    return fib(*test)
```

Results in (after running `objdump -d output.o -M intel`):
```
output.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <fib>:
   0:   83 ff 02                cmp    edi,0x2
   3:   7f 06                   jg     b <fib+0xb>
   5:   b8 01 00 00 00          mov    eax,0x1
   a:   c3                      ret
   b:   55                      push   rbp
   c:   53                      push   rbx
   d:   50                      push   rax
   e:   89 fb                   mov    ebx,edi
  10:   8d 7b ff                lea    edi,[rbx-0x1]
  13:   e8 00 00 00 00          call   18 <fib+0x18>
  18:   89 c5                   mov    ebp,eax
  1a:   83 c3 fe                add    ebx,0xfffffffe
  1d:   89 df                   mov    edi,ebx
  1f:   e8 00 00 00 00          call   24 <fib+0x24>
  24:   01 e8                   add    eax,ebp
  26:   48 83 c4 08             add    rsp,0x8
  2a:   5b                      pop    rbx
  2b:   5d                      pop    rbp
  2c:   c3                      ret
  2d:   0f 1f 00                nop    DWORD PTR [rax]

0000000000000030 <main>:
  30:   50                      push   rax
  31:   c7 04 25 00 00 01 00    mov    DWORD PTR ds:0x10000,0x19
  38:   19 00 00 00
  3c:   bf 19 00 00 00          mov    edi,0x19
  41:   e8 00 00 00 00          call   46 <main+0x16>
  46:   59                      pop    rcx
  47:   c3                      ret
```
