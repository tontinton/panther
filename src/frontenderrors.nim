type
    ErrorInfo* = ref object
        line*: int
        lineStart*: int
        start*: int
        length*: int

    Errorable = concept e
        e.error is seq[ErrorInfo]

    ParseError* = ref object of CatchableError
        info*: seq[ErrorInfo]
        next*: ParseError

func newErrorInfo*(line, lineStart, start, length: int): ErrorInfo =
    ErrorInfo(line: line, lineStart: lineStart, start: start, length: length)

template newParseError*[E: Errorable](e: E, message: string): ParseError =
    ParseError(info: e.error, next: nil, msg: message, parent: nil)