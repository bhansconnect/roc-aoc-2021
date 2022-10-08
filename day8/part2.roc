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
            |> List.map (\entry ->
                split = Str.split entry " | "
                digits =
                    split
                    |> List.first
                    |> Result.withDefault ""
                    |> Str.split " "
                    |> List.map strSet
                output =
                    split
                    |> List.dropFirst
                    |> List.first
                    |> Result.withDefault ""
                    |> Str.split " "
                    |> List.map strSet
                {digits, output}
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

processData : List {digits: List (Set U8), output: List (Set U8)} -> U64
processData = \data ->
    data
    |> List.map processEntry
    |> List.sum

processEntry : {digits: List (Set U8), output: List (Set U8)} -> U64
processEntry = \{digits, output} ->
    one =
        digits
        |> List.findFirst (\x -> 2 == Set.len x)
        |> setOrCrash
    seven =
        digits
        |> List.findFirst (\x -> 3 == Set.len x)
        |> setOrCrash
    four =
        digits
        |> List.findFirst (\x -> 4 == Set.len x)
        |> setOrCrash
    eight =
        digits
        |> List.findFirst (\x -> 7 == Set.len x)
        |> setOrCrash
    zeroSixNine =
        digits
        |> List.keepIf (\x -> 6 == Set.len x)
    nine =
        zeroSixNine
        |> List.findFirst (\x -> Set.union x four |> setEq x)
        |> setOrCrash
    diffEightSeven = Set.difference eight seven
    six =
        zeroSixNine
        |> List.findFirst (\x -> Set.union x diffEightSeven |> setEq x)
        |> setOrCrash
    zero =
        zeroSixNine
        |> List.findFirst (\x -> x != six && x != nine)
        |> setOrCrash
    twoThreeFive =
        digits
        |> List.keepIf (\x -> 5 == Set.len x)
    three =
        twoThreeFive
        |> List.findFirst (\x -> Set.union x seven |> setEq x)
        |> setOrCrash
    five =
        twoThreeFive
        |> List.findFirst (\x -> 1 == (Set.difference six x |> Set.len))
        |> setOrCrash
    two =
        twoThreeFive
        |> List.findFirst (\x -> x != three && x != five)
        |> setOrCrash
    output
    |> List.map (\x ->
        if setEq x zero then
            0
        else if setEq x one then
            1
        else if setEq x two then
            2
        else if setEq x three then
            3
        else if setEq x four then
            4
        else if setEq x five then
            5
        else if setEq x six then
            6
        else if setEq x seven then
            7
        else if setEq x eight then
            8
        else if setEq x nine then
            9
        else
            0 - 1
    )
    |> List.walk 0 (\accum, num ->
        10 * accum + num
    )

setEq : Set a, Set a -> Bool
setEq = \x, y ->
    if Set.len x == Set.len y then
        Set.union x y
        |> Set.len
        |> (\union -> union == Set.len x)
    else
        Bool.false

setOrCrash: Result (Set U8) err -> Set U8
setOrCrash = \result ->
    when result is
        Ok set -> set
        Err _ -> Set.fromList [0 - 1]


strSet : Str -> Set U8
strSet = \str ->
    Str.walkUtf8WithIndex str Set.empty (\set, byte, _ -> Set.insert set byte)