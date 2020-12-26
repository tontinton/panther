# Package

version       = "0.1.0"
author        = "Tony Solomonik"
description   = "The panther language compiler"
license       = "MIT"
srcDir        = "src"
bin           = @["panther"]



# Dependencies

requires "nim >= 1.3.7"
requires "optionsutils == 1.2.0"
requires "cligen == 1.3.2"
