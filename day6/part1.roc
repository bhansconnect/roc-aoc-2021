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
    List.walk data (List.repeat 0 9) (\counts, fishAgeU64 ->
        fishAge = Num.toNat fishAgeU64
        List.set counts fishAge ((listGetOrZero counts fishAge) + 1)
    )
    |> repeat runDay 80
    |> List.sum

runDay : List U64 -> List U64
runDay = \fish ->
    births = listGetOrZero fish 0
    fish
    |> List.dropFirst
    |> List.append births
    |> (\list -> List.set list 6 ((listGetOrZero list 6) + births))

repeat : a, (a -> a), U64 -> a
repeat = \data, fn, remaining ->
    if remaining > 0 then
        repeat (fn data) fn (remaining - 1)
    else
        data

listGetOrZero : List U64, Nat -> U64
listGetOrZero = \list, i -> List.get list i |> Result.withDefault 0