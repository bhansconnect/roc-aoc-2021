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
    oxygen = filterData data 0 Bool.true
    co2 = filterData data 0 Bool.false
    when T (List.first oxygen) (List.first co2) is
        T (Ok x) (Ok y) -> (toDecimal x) * (toDecimal y)
        _ ->
            # This shouldn't be possible, just panic.
            # I should probably bubble up results cleanly and do this right.
            x : U64
            x = 0 - 1
            x

filterData : List (List U64), Nat, Bool -> List (List U64) 
filterData = \data, i, keepCommon ->
    len = Num.toU64 (List.len data)
    if len == 1 then
        data
    else
        count = List.walk data 0 (\accum, currentBits ->
            when List.get currentBits i is
                Ok bit ->
                    bit + accum
                Err _ ->
                    # This shouldn't be possible, just panic.
                    # I should probably bubble up results cleanly and do this right.
                    x : U64
                    x = 0 - 1
                    x
            )
        commonBit = if count >= len - count then 1 else 0
        filtered = List.keepIf data (\currentBits ->
            when List.get currentBits i is
                Ok bit ->
                    (keepCommon && bit == commonBit) || (!keepCommon && bit != commonBit)
                Err _ ->
                    # Same here, this should fail and bubble up.
                    # Though again. it isn't possible.
                    Bool.false
            )
        filterData filtered (i + 1) keepCommon

toDecimal : List U64 -> U64
toDecimal = \list ->
    accum, bit <- List.walk list 0
    bit + Num.shiftLeftBy accum 1
