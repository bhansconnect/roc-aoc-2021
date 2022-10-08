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
    initialCost =
        List.map2 counts (List.range 0 max) (\count, dist -> count * Num.toU64 dist)
        |> List.sum
    firstCount = List.first counts |> Result.withDefault 0
    remainingCounts = List.dropFirst counts
    List.walk remainingCounts {bestCost: initialCost, runningCost: initialCost, left: firstCount, right: List.sum remainingCounts} (\state, count ->
        # Everything on left is on farther, everything on right is one closer.
        cost = state.runningCost + state.left - state.right
        {
            left: state.left + count,
            right: state.right - count,
            runningCost: cost,
            bestCost: if cost < state.bestCost then cost else state.bestCost,
        }
    ) |> .bestCost

incList : List U64, U64 -> List U64
incList = \list, i ->
    iNat = Num.toNat i
    when List.get list iNat is
        Ok n ->
            List.set list iNat (n + 1)
        Err _ ->
            # This should be impossible for our case
            List.set list iNat (0 - 1)