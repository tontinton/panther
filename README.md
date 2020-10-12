# IN DEVELOPMENT

![Nim](https://github.com/tontinton/panther/workflows/Nim/badge.svg?branch=master)

## Introduction
The panther language is a language made for exploitations.

It's main focus is **minimal code size** through code size 
optimizations and compiling directly to a **shellcode** (position independent code).

## What can it do currently? 
Currently, the panther binary can parse a single file to an AST representation, with minimal type inference and type checking.

When running the panther compiler on test.pan
```bash
nimble build -d:release
./panther test.pan
```

when ``test.pan`` looks like:
```nim
proc fib(n: s32) -> s32:
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

proc isFoo(input: string) -> bool:
    if input == "foo":
        return true
    else:
        return false

proc main() -> s32:
    let x : s32 = 2 + 5 * 8
    let y = fib(x - 30)  # auto type inference
    if x + 123 * (y + 7) > 100 and isFoo("bar"):
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
            type: (kind: Signed32)
            ident: n
          ),
        ]
      return type: (kind: Signed32)
      implementation:
        [
          (
            if:
              <=:
                left:
                  ident: n
                right:
                  type: (kind: Signed32)
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
                                    type: (kind: Signed32)
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
                                    type: (kind: Signed32)
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
      name: isFoo
      params:
        [
          (
            type: (kind: String)
            ident: input
          ),
        ]
      return type: (kind: Boolean)
      implementation:
        [
          (
            if:
              ==:
                left:
                  ident: input
                right:
                  type: (kind: String)
                  literal: foo
            then:
              [
                (
                  return:
                    type: (kind: Boolean)
                    literal: true
                ),
              ]
            else:
              [
                (
                  return:
                    type: (kind: Boolean)
                    literal: false
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
      return type: (kind: Signed32)
      implementation:
        [
          (
            declaration:
              =:
                asignee:
                  type: (kind: Signed32)
                  ident: x
                value:
                  +:
                    left:
                      type: (kind: Signed32)
                      literal: 2
                    right:
                      *:
                        left:
                          type: (kind: Signed32)
                          literal: 5
                        right:
                          type: (kind: Signed32)
                          literal: 8
          ),
          (
            declaration:
              =:
                asignee:
                  type: (kind: Signed32)
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
                              type: (kind: Signed32)
                              literal: 30
                        ),
                      ]
          ),
          (
            if:
              and:
                left:
                  >:
                    left:
                      +:
                        left:
                          ident: x
                        right:
                          *:
                            left:
                              type: (kind: Signed32)
                              literal: 123
                            right:
                              +:
                                left:
                                  ident: y
                                right:
                                  type: (kind: Signed32)
                                  literal: 7
                    right:
                      type: (kind: Signed32)
                      literal: 100
                right:
                  function call:
                    name: isFoo
                    params:
                      [
                        (
                          type: (kind: String)
                          literal: bar
                        ),
                      ]
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
                    type: (kind: Signed32)
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
                      type: (kind: Signed32)
                      literal: 0
                  ),
                ]
          ),
        ]
  ),
]
```