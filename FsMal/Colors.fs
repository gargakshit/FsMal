namespace FsMal

// Adapted from https://github.com/sindresorhus/yoctocolors/blob/b0074980c27de64430085aee94752b0deabe3d65/index.js

module Colors =
    let format startCode endCode str =
        "\u001B["
        + startCode.ToString()
        + "m"
        + str
        + "\u001B["
        + endCode.ToString()
        + "m"

    let reset = format 0 0
    let bold = format 1 22
    let dim = format 2 22
    let italic = format 3 23
    let underline = format 4 24
    let overline = format 53 55
    let inverse = format 7 27
    let hidden = format 8 28
    let strikethrough = format 9 29

    let black = format 30 39
    let red = format 31 39
    let green = format 32 39
    let yellow = format 33 39
    let blue = format 34 39
    let magenta = format 35 39
    let cyan = format 36 39
    let white = format 37 39
    let gray = format 90 39

    let bgBlack = format 40 49
    let bgRed = format 41 49
    let bgGreen = format 42 49
    let bgYellow = format 43 49
    let bgBlue = format 44 49
    let bgMagenta = format 45 49
    let bgCyan = format 46 49
    let bgWhite = format 47 49
    let bgGray = format 100 49
