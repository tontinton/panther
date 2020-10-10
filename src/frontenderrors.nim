type
    ErrorInfo* = ref object
        line*: int
        lineStart*: int
        start*: int
        length*: int

    Errorable = concept e
        e.error is seq[ErrorInfo]

    ParserError* = ref object of CatchableError
        info*: seq[ErrorInfo]

func newErrorInfo*(line, lineStart, start, length: int): ErrorInfo =
    ErrorInfo(line: line, lineStart: lineStart, start: start, length: length)

template newParseError*[E: Errorable](e: E, message: string): ParserError =
    ParserError(info: e.error, msg: message, parent: nil)
