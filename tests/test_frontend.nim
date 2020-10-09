import unittest

import frontend
import ast

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
]"""

suite "frontend":
    test "sanity":
        check(EXPECTED_OUTPUT == $INPUT.parse())
