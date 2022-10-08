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
            |> List.mapTry Str.toI64
        when parseResult is
            Ok data ->
                count = processData data
                countStr = Num.toStr count
                Stdout.line "Found \(countStr) increases"
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

processData : List I64 -> Nat
processData = \data ->
    movingSums =
        drop1 = (List.dropFirst data)
        drop2 = (List.dropFirst drop1)
        List.map3 data drop1 drop2 (\a,b,c -> a + b + c)
    List.map2 movingSums (List.dropFirst movingSums) Num.isLt
    |> List.keepIf (\x -> x)
    |> List.len
