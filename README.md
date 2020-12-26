# IN DEVELOPMENT

![Nim](https://github.com/tontinton/panther/workflows/Nim/badge.svg?branch=master)

## Introduction
The panther language is a language made for exploitations.

It's main focus is **minimal code size** through code size 
optimizations and compiling directly to a **shellcode** (position independent code).

## What can it do currently? 
Currently, the panther binary can compile a single file using the llvm backend, with minimal type inference and type checking.

When running the panther compiler on test.pan
```bash
nimble build -d:release
./panther c main.pan
```

When ``test.pan`` looks like:
```nim
proc fib(a: s32) -> s32:
    if a <= 2:
        return 1
    return fib(a - 1) + fib(a - 2)

proc main() -> s32:
    let a = 50
    return fib(a)
```

Results in (after running `objdump -d output.o`):
```
output.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <^C>:
   0:   55                      push   %rbp
   1:   53                      push   %rbx
   2:   50                      push   %rax
   3:   89 fb                   mov    %edi,%ebx
   5:   8d 7b ff                lea    -0x1(%rbx),%edi
   8:   e8 00 00 00 00          callq  d <^C+0xd>
   d:   89 c5                   mov    %eax,%ebp
   f:   83 c3 fe                add    $0xfffffffe,%ebx
  12:   89 df                   mov    %ebx,%edi
  14:   e8 00 00 00 00          callq  19 <^C+0x19>
  19:   01 e8                   add    %ebp,%eax
  1b:   48 83 c4 08             add    $0x8,%rsp
  1f:   5b                      pop    %rbx
  20:   5d                      pop    %rbp
  21:   c3                      retq
  22:   66 2e 0f 1f 84 00 00    nopw   %cs:0x0(%rax,%rax,1)
  29:   00 00 00
  2c:   0f 1f 40 00             nopl   0x0(%rax)

0000000000000030 <^D>:
  30:   50                      push   %rax
  31:   bf 32 00 00 00          mov    $0x32,%edi
  36:   e8 00 00 00 00          callq  3b <^D+0xb>
  3b:   59                      pop    %rcx
  3c:   c3                      retq
```