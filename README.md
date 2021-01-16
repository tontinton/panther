## Introduction
The panther language is a language made for exploitations.

It's main focus is **minimal code size** through code size 
optimizations and compiling directly to a **shellcode** (position independent code).

## What can it do currently? 
Currently, the panther binary can compile a single file using the llvm backend, with minimal type inference and type checking.

When running the panther compiler on main.pan
```bash
nimble build -d:release
./panther c -i main.pan
```

When ``main.pan`` looks like:
```nim
proc fib(a: s32) -> s32:
    if a <= 2:
        return 1
    return fib(a - 1) + fib(a - 2)

proc main() -> s32:
    let a = 50
    return fib(a)
```

Results in (after running `objdump -d output.o -M intel`):
```
output.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <fib>:
   0:   55                      push   rbp
   1:   53                      push   rbx
   2:   50                      push   rax
   3:   89 fb                   mov    ebx,edi
   5:   8d 7b ff                lea    edi,[rbx-0x1]
   8:   e8 00 00 00 00          call   d <fib+0xd>
   d:   89 c5                   mov    ebp,eax
   f:   83 c3 fe                add    ebx,0xfffffffe
  12:   89 df                   mov    edi,ebx
  14:   e8 00 00 00 00          call   19 <fib+0x19>
  19:   01 e8                   add    eax,ebp
  1b:   48 83 c4 08             add    rsp,0x8
  1f:   5b                      pop    rbx
  20:   5d                      pop    rbp
  21:   c3                      ret
  22:   66 2e 0f 1f 84 00 00    nop    WORD PTR cs:[rax+rax*1+0x0]
  29:   00 00 00
  2c:   0f 1f 40 00             nop    DWORD PTR [rax+0x0]

0000000000000030 <main>:
  30:   50                      push   rax
  31:   bf 32 00 00 00          mov    edi,0x32
  36:   e8 00 00 00 00          call   3b <main+0xb>
  3b:   59                      pop    rcx
  3c:   c3                      ret
```
