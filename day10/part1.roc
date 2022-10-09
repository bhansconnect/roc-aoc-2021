app "exec-part1"
    packages { pf: "../../roc/examples/cli/cli-platform/main.roc" }
    imports [
        pf.Program.{ Program, ExitCode },
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.File,
        pf.Path,
        # pf.Env,
        # pf.Dir,
    ]
    provides [main] to pf

main : Program
main = Program.withArgs \fullArgs ->
    # First drop the executable.
    args = List.dropFirst fullArgs
    when List.first args is
        Ok fileName ->
            Path.fromStr fileName |> mainTask
        Err _ ->
            Stderr.line "Expected a file name passed on the command line"
            |> Program.exit 1

mainTask : Path.Path -> Task ExitCode [] [Write [Stdout, Stderr], Read [File]]
mainTask = \inputPath ->
    task =
        contents <- File.readUtf8 inputPath |> Task.await
        data =
            contents
            |> Str.trim
            |> Str.split "\n"
            |> List.map Str.toUtf8
            |> List.map (\bytes ->
                bytes
                |> List.map (\byte ->
                    when Num.toU32 byte is
                        '[' -> OpenSquare
                        '{' -> OpenSquiggle
                        '(' -> OpenParen
                        '<' -> OpenTriangle
                        ']' -> CloseSquare
                        '}' -> CloseSquiggle
                        ')' -> CloseParen
                        '>' -> CloseTriangle
                        _ -> crash OpenParen
                )
            )

        out = processData data
        outStr = Num.toStr out
        Stdout.line "\(outStr)"


    Task.attempt task \result ->
        when result is
            Ok {} ->
                Task.succeed {}
                |> Program.exit 0

            Err err ->
                inputPathStr = Path.display inputPath
                msg =
                    when err is
                        FileReadErr _ _ ->
                            "Failed to read file: \(inputPathStr)"
                        ParseError ->
                            "Failed to parse file: \(inputPathStr)"
                        _ ->
                            "Uh oh, there was an error!"

                Stderr.line msg
                |> Program.exit 1

Bracket : [ 
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    OpenSquiggle,
    CloseSquiggle,
    OpenTriangle,
    CloseTriangle,
]

processData : List (List Bracket) -> U64
processData = \data ->
    data
    |> List.map processLine
    |> List.map (\res ->
        when res is
            Ok -> 0
            Bad CloseParen -> 3
            Bad CloseSquare -> 57 
            Bad CloseSquiggle -> 1197
            Bad CloseTriangle -> 25137
            _ -> crash 0
    )
    |> List.sum

processLine : List Bracket -> [Ok, Bad Bracket]
processLine = \brackets ->
    List.walkUntil brackets {stack: [], result: Ok} (\{stack, result}, bracket ->
        when bracket is
            OpenParen | OpenSquare | OpenSquiggle | OpenTriangle ->
                Continue {stack: List.append stack bracket, result}
            _ ->
                last =
                    when List.last stack is
                        Ok x -> x
                        Err _ -> crash OpenParen
                if bracket != flipBracket last then
                    Break {stack, result: Bad bracket}
                else
                    Continue {stack: List.dropLast stack, result}
    )
    |> .result

flipBracket : Bracket -> Bracket
flipBracket = \bracket ->
    when bracket is
        OpenParen -> CloseParen
        CloseParen -> OpenParen
        OpenSquare -> CloseSquare
        CloseSquare -> OpenSquare
        OpenSquiggle -> CloseSquiggle
        CloseSquiggle -> OpenSquiggle
        OpenTriangle -> CloseTriangle
        CloseTriangle -> OpenTriangle

crash : a -> a 
crash = \a -> crashInternal (0 - 1) a

crashInternal : U8, a -> a
crashInternal = \_, a -> a
