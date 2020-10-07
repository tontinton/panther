# IN DEVELOPMENT

![Nim](https://github.com/tontinton/panther/workflows/Nim/badge.svg?branch=master)

## Introduction
The panther language is a language made for exploitations.

It's main focus is **minimal code size** through code size 
optimizations and compiling directly to a **shellcode** (position independent code).

## What can it do currently? 
Currently, the panther binary can only parse a single file to an AST representation.

When running the panther compiler on test.pan
```bash
nim build -d:release
./panther test.pan
```

when ``test.pan`` looks like:
```nim
proc fib(n: u32) -> u32:
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

proc main() -> u32:
    let x : u32 = 2 + 5 * 8
    let y = fib(x - 30)  # auto type inference
    if x * y > 100:
        return x + y
    elif x * y < 100:
        return x - y
    else:
        return 0
```

The result is:
```yaml
[
  (
    function:
      name: fib
      params:
        [
          (
            type: (kind: Undetermined, value: "u32")
            ident: n
          ),
        ]
      return type: (kind: Undetermined, value: "u32")
      implementation:
        [
          (
            if:
              <=:
                left:
                  ident: n
                right:
                  literal: 1
            then:
              [
                (
                  return:
                    ident: n
                ),
              ]
            else:
              [
                (
                  return:
                    +:
                      left:
                        function call:
                          name: fib
                          params:
                            [
                              (
                                -:
                                  left:
                                    ident: n
                                  right:
                                    literal: 1
                              ),
                            ]
                      right:
                        function call:
                          name: fib
                          params:
                            [
                              (
                                -:
                                  left:
                                    ident: n
                                  right:
                                    literal: 2
                              ),
                            ]
                ),
              ]
          ),
        ]
  ),
  (
    empty
  ),
  (
    function:
      name: main
      params:
        [
        ]
      return type: (kind: Undetermined, value: "u32")
      implementation:
        [
          (
            declaration:
              =:
                asignee:
                  type: (kind: Undetermined, value: "u32")
                  ident: x
                value:
                  +:
                    left:
                      literal: 2
                    right:
                      *:
                        left:
                          literal: 5
                        right:
                          literal: 8
          ),
          (
            declaration:
              =:
                asignee:
                  ident: y
                value:
                  function call:
                    name: fib
                    params:
                      [
                        (
                          -:
                            left:
                              ident: x
                            right:
                              literal: 30
                        ),
                      ]
          ),
          (
            if:
              >:
                left:
                  *:
                    left:
                      ident: x
                    right:
                      ident: y
                right:
                  literal: 100
            then:
              [
                (
                  return:
                    +:
                      left:
                        ident: x
                      right:
                        ident: y
                ),
              ]
            else:
              if:
                <:
                  left:
                    *:
                      left:
                        ident: x
                      right:
                        ident: y
                  right:
                    literal: 100
              then:
                [
                  (
                    return:
                      -:
                        left:
                          ident: x
                        right:
                          ident: y
                  ),
                ]
              else:
                [
                  (
                    return:
                      literal: 0
                  ),
                ]
          ),
        ]
  ),
]
```