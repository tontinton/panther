import unittest
import options

import frontend/frontend
import frontend/ast

const INPUT = readFile("test.pan")

const EXPECTED_OUTPUT = """
[
  (
    function:
      name: fib
      params:
        [
          (
            type: Signed32
            ident: n
          ),
        ]
      return type: Signed32
      extern: false
      implementation:
        [
          (
            if:
              <=:
                left:
                  ident: n
                right:
                  type: Signed32
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
                                    type: Signed32
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
                                    type: Signed32
                                    literal: 2
                              ),
                            ]
                ),
              ]
          ),
          (
            return:
              type: Signed32
              literal: 0
          ),
        ]
  ),
  (
    function:
      name: isFoo
      params:
        [
          (
            type: String
            ident: input
          ),
        ]
      return type: Boolean
      extern: false
      implementation:
        [
          (
            if:
              ==:
                left:
                  ident: input
                right:
                  type: String
                  literal: foo
            then:
              [
                (
                  return:
                    type: Boolean
                    literal: true
                ),
              ]
            else:
              [
                (
                  return:
                    type: Boolean
                    literal: false
                ),
              ]
          ),
          (
            return:
              type: Boolean
              literal: false
          ),
        ]
  ),
  (
    function:
      name: main
      params:
        [
        ]
      return type: Signed32
      extern: false
      implementation:
        [
          (
            declaration:
              =:
                asignee:
                  type: Signed32
                  ident: x
                value:
                  +:
                    left:
                      type: Signed32
                      literal: 2
                    right:
                      *:
                        left:
                          type: Signed32
                          literal: 5
                        right:
                          type: Signed32
                          literal: 8
          ),
          (
            declaration:
              =:
                asignee:
                  type: Signed32
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
                              type: Signed32
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
                              type: Signed32
                              literal: 123
                            right:
                              +:
                                left:
                                  ident: y
                                right:
                                  type: Signed32
                                  literal: 7
                    right:
                      type: Signed32
                      literal: 100
                right:
                  not:
                    function call:
                      name: isFoo
                      params:
                        [
                          (
                            type: String
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
                    type: Signed32
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
                      type: Signed32
                      literal: 0
                  ),
                ]
          ),
          (
            return:
              type: Signed32
              literal: 0
          ),
        ]
  ),
]"""

suite "frontend":
    test "sanity":
        let output = INPUT.parseText()
        check:
            output.isSome()
            EXPECTED_OUTPUT == $output.get()

    test "invalidity":
        check:
            "x -> 2".parseText().isNone()  # invalid syntax
            "let x = \"hello\";let y = x + 2;".parseText().isNone()  # types differ
