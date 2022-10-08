app "exec-part2"
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
        parseResult =
            contents
            |> Str.trim
            |> Str.split "\n"
            |> List.map (\x -> Str.split x " ")
            |> List.mapTry (\sublist ->
                when T (List.first sublist) (List.last sublist) is
                    T (Ok cmd) (Ok numStr) ->
                        when Str.toI64 numStr is
                            Ok num ->
                                Ok (T cmd num)
                            _ ->
                                Err ParseError
                    _ ->
                        Err ParseError
            )
        when parseResult is
            Ok data ->
                out = processData data
                outStr = Num.toStr out
                Stdout.line "\(outStr)"
            Err _ ->
                Task.fail (ParseError)


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

processData : List [T Str I64] -> I64
processData = \data ->
    # I feel like doing this a silly map way instead of with a walk at first.
    List.map data (\T cmd num ->
        when cmd is
            "up" -> T -num 0
            "down" -> T num 0
            "forward" -> T 0 num
            _ -> T 0 0
    ) |> List.walk (T 0 0 0) (\T depth fwd aim, T deltaAim deltaFwd ->
        T (depth + deltaFwd * aim) (fwd + deltaFwd) (aim + deltaAim)
    ) |> (\T depth fwd _ -> depth * fwd)
