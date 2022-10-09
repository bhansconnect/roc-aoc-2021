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
        width =
            contents
            |> Str.trim
            |> Str.split "\n"
            |> List.first
            |> Result.withDefault ""
            |> Str.countUtf8Bytes
        data =
            contents
            |> Str.trim
            |> Str.replaceEach "\n" ""
            |> Result.withDefault ""
            |> Str.toUtf8
            |> List.map (\x -> x - 48)

        out = processData {data, width}
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

Matrix : {data: List U8, width: Nat}
Pos : {row: Nat, col: Nat}

processData : Matrix -> U64
processData = \m ->
    List.len m.data
    |> (\len -> List.range 0 len)
    |> List.keepIf (\i ->
        pos = indexToPos m i
        u = up m pos
        d = down m pos
        l = left m pos
        r = right m pos
        v = List.get m.data i |> okOrCrash
        v < u && v < d && v < l && v < r
    )
    |> List.map (\i -> getBasinSize m i)
    |> List.sortDesc
    |> List.takeFirst 3
    |> List.product

getBasinSize : Matrix, Nat -> U64
getBasinSize = \m, i ->
    indexToPos m i
    |> (\pos -> getBasinHelper m Set.empty [pos])
    |> Set.len
    |> Num.toU64

getBasinHelper : Matrix, Set Pos, List Pos -> Set Pos
getBasinHelper = \m, seen, waiting ->
    if List.isEmpty waiting then
        seen
    else
        last =
            List.last waiting
            |> okOrCrash
        v =
            posToIndex m last
            |> (\i -> List.get m.data i)
            |> okOrCrash
        getNeighbors m last
        |> List.dropIf (\pos -> Set.contains seen pos)
        |> List.keepIf (\pos ->
            posToIndex m pos
            |> (\i -> List.get m.data i)
            |> okOrCrash
            |> (\x -> x > v && x != 9)
        )
        |> List.walk (List.dropLast waiting) List.append
        |> (\nextWaiting -> getBasinHelper m (Set.insert seen last) nextWaiting)

getNeighbors : Matrix, Pos -> List Pos
getNeighbors = \m, {row, col} ->
    height = List.len m.data |> Num.divTrunc m.width
    l0 = List.withCapacity 4
    l1 = 
        if row > 0 then
            List.append l0 {row: row - 1, col}
        else
            l0
    l2 = 
        if row < height - 1 then
            List.append l1 {row: row + 1, col}
        else
            l1
    l3 = 
        if col > 0 then
            List.append l2 {row, col: col - 1}
        else
            l2
    l4 = 
        if col < m.width - 1 then
            List.append l3 {row, col: col + 1}
        else
            l3
    l4

up : Matrix, Pos -> U8
up = \m, {row, col} ->
    if row > 0 then
        i = posToIndex m {row: row - 1, col}
        List.get m.data i
        |> okOrCrash
    else
        Num.maxU8

down : Matrix, Pos -> U8
down = \m, {row, col} ->
    height = List.len m.data |> Num.divTrunc m.width
    if row < height - 1 then
        i = posToIndex m {row: row + 1, col}
        List.get m.data i
        |> okOrCrash
    else
        Num.maxU8

left : Matrix, Pos -> U8
left = \m, {row, col} ->
    if col > 0 then
        i = posToIndex m {row, col: col - 1}
        List.get m.data i
        |> okOrCrash
    else
        Num.maxU8

right : Matrix, Pos -> U8
right = \m, {row, col} ->
    if col < m.width - 1 then
        i = posToIndex m {row, col: col + 1}
        List.get m.data i
        |> okOrCrash
    else
        Num.maxU8



indexToPos : Matrix, Nat -> Pos
indexToPos = \{width}, i ->
    row = Num.divTrunc i width
    col = i % width
    {row, col}

posToIndex : Matrix, Pos -> Nat
posToIndex = \{width}, {row, col} ->
    row * width + col

okOrCrash : Result a err -> a
okOrCrash = \result ->
    when result is
        Ok a -> a
        Err _ -> crash {}

crash : {} -> * 