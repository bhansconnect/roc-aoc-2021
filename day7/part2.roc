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
        data =
            contents
            |> Str.trim
            |> Str.split ","
            |> List.mapTry Str.toU64
            |> Result.withDefault []

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

processData : List U64 -> U64
processData = \data ->
    max =
        List.max data
        |> Result.withDefault 0
        |> Num.toNat
        |> Num.add 1
    counts = List.walk data (List.repeat 0 max) incList
    genCosts max
    |> List.map (\costs ->
        List.map2 counts costs (\count, cost -> count * cost)
        |> List.sum
    )
    |> List.min
    |> Result.withDefault 0

incList : List U64, U64 -> List U64
incList = \list, i ->
    iNat = Num.toNat i
    when List.get list iNat is
        Ok n ->
            List.set list iNat (n + 1)
        Err _ ->
            # This should be impossible for our case
            List.set list iNat (0 - 1)


genCosts : Nat -> List (List U64)
genCosts = \max ->
    List.range 0 max
    |> List.map (\i ->
        genCost max i
    )

genCost : Nat, Nat -> List U64
genCost = \max, target ->
    List.repeat 0 max
    |> List.mapWithIndex (\_, i -> Num.abs (Num.toI64 target - Num.toI64 i))
    |> List.map (\x -> x * (x + 1) |> Num.divTrunc 2)
    |> List.map Num.toU64