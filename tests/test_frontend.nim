import unittest

import lexer
import parser
import ast
import analyzer

const INPUT = readFile("test.pan")

const EXPECTED_OUTPUT = """
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
]"""

suite "frontend":
    test "sanity":
        let expression = newParser().parseBlock(newLexer(INPUT).tokens())
        expression.analyze()
        check(EXPECTED_OUTPUT == $expression)
