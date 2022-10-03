app "exec-part1"
    packages { pf: "../../roc/examples/interactive/cli-platform/main.roc" }
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
        toPoint : Str -> Point
        toPoint = \str ->
            # This could really use list pattern matching.
            indices =
                str
                |> Str.split ","
                |> List.mapTry Str.toU64
                |> Result.withDefault [0, 0]
            x =
                indices
                |> List.first
                |> Result.withDefault 0
            y =
                indices
                |> List.last
                |> Result.withDefault 0
            Point x y

        lines =
            contents
            |> Str.trim
            |> Str.split "\n"
            |> List.map (\line ->
                points =
                    line
                    |> Str.split " -> "
                    |> List.map toPoint
                start =
                    points
                    |> List.first
                    |> Result.withDefault (Point 0 0)
                end =
                    points
                    |> List.last
                    |> Result.withDefault (Point 0 0)
                Line start end
            )
        
        out = processData lines
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

Point : [Point U64 U64]
Line : [Line Point Point]

processData : List Line -> U64
processData = \lines ->
    covered =
        lines
        |> List.keepIf (\Line (Point x1 y1) (Point x2 y2) -> Bool.or (x1 == x2) (y1 == y2))
        |> List.map coveredPoints
    List.walk covered {accum: Set.empty, remaining: List.dropFirst covered} (\{accum, remaining}, current ->
        overlaps =
            remaining
            |> List.map (\set -> Set.intersection current set)
            |> List.walk Set.empty Set.union
        {accum: Set.union accum overlaps, remaining: List.dropFirst remaining}
    )
    |> \{accum} -> accum
    |> Set.len
    |> Num.toU64

coveredPoints : Line -> Set Point
coveredPoints = \Line (Point x1 y1) (Point x2 y2) ->
    if x1 == x2 then
        betterRange y1 y2
        |> List.map (\y -> Point x1 y)
        |> Set.fromList
    else if y1 == y2 then
        betterRange x1 x2
        |> List.map (\x -> Point x y1)
        |> Set.fromList
    else
        # Diagonal not support for this part.
        Set.empty

betterRange : U64, U64 -> List U64
betterRange = \start, end ->
    # List.range docs are wrong.
    if start == end then
        [start]
    else if start < end then
        List.range start (end + 1)
    else
        List.range end (start + 1)
        |> List.reverse
