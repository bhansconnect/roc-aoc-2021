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
        data =
            contents
            |> Str.trim
            |> Str.split "\n"
            |> List.map Str.toUtf8
            # Convert from utf8 to real numbers
            |> List.map (\l -> List.map l (\x -> (Num.toU64 x) - 48))
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

processData : List (List U64) -> U64
processData = \data ->
    len = Num.toU64 (List.len data)
    List.walk data [] (\accum, next ->
        if List.isEmpty accum then
            next
        else
            List.map2 accum next Num.add
    ) |> List.walk (T 0 0) (\T gamma epsilon, bitCount ->
        shiftedGamma = Num.shiftLeftBy gamma 1
        shiftedEpsilon = Num.shiftLeftBy epsilon 1
        if bitCount > len - bitCount then
            T (shiftedGamma + 1) shiftedEpsilon
        else
            T shiftedGamma (shiftedEpsilon + 1)
    ) |> (\T gamma epsilon -> gamma * epsilon)