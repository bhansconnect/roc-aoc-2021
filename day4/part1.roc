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
        # I think using a parser libary like Ayaz is the better way to do this.
        # I am implementing this is the string splitting way to test how that would be to implement in Roc.
        groupedLines =
            contents
            |> Str.trim
            |> Str.split "\n"
            |> splitListOnEmptyLine
        
        pickedNumbers =     
            List.first groupedLines
            |> Result.withDefault [""]
            |> List.first
            |> Result.withDefault ""
            |> Str.split ","
            |> List.mapTry Str.toU64
        
        boardLists =
            groupedLines
            |> List.dropFirst
            |> List.mapTry (\lines -> 
                lines
                |> Str.joinWith " "
                |> Str.split " "
                |> List.map Str.trim
                |> List.dropIf Str.isEmpty
                |> List.mapTry Str.toU64
            )
        when T pickedNumbers boardLists is
            T (Ok nums) (Ok boards) ->
                out = processData nums boards
                outStr = Num.toStr out
                Stdout.line "\(outStr)"
            _ ->
                Task.fail ParseError


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

splitListOnEmptyLine : List Str -> List (List Str)
splitListOnEmptyLine = \list ->
    List.walk list {accum: [], current: []} (\{accum, current}, line ->
        if Str.isEmpty line then
            {accum: List.append accum current, current: []}
        else
            {accum, current: List.append current line}
    ) |> \{accum, current} -> List.append accum current

boardRows : List U64 -> List (List U64)
boardRows = \board ->
    [
        List.sublist board {start: 0, len: 5},
        List.sublist board {start: 5, len: 5},
        List.sublist board {start: 10, len: 5},
        List.sublist board {start: 15, len: 5},
        List.sublist board {start: 20, len: 5},
    ]

boardCols : List U64 -> List (List U64)
boardCols = \board ->
    # There sadly is no List.map5 so instead using map4 and then map1
    rows = boardRows board
    r0 = List.get rows 0 |> Result.withDefault []
    r1 = List.get rows 1 |> Result.withDefault []
    r2 = List.get rows 2 |> Result.withDefault []
    r3 = List.get rows 3 |> Result.withDefault []
    r4 = List.get rows 4 |> Result.withDefault []
    List.map4 r0 r1 r2 r3 (\e0, e1, e2, e3 -> [e0, e1, e2, e3])
    |> List.map2 r4 (\row, e4 -> List.append row e4)

boardRecord : List U64 -> {board: List U64, rowscols: List (List U64)}
boardRecord = \board ->
    rows = boardRows board
    cols = boardCols board
    rowscols = List.concat rows cols
    {board, rowscols}

processData : List U64, List (List U64) -> U64
processData = \numbers, boards ->
    boardsData = List.map boards boardRecord
    state = 
        numbers
        |> List.split 5
        |> \{before, others} -> {seen: before, remaining: others}
    processDataHelper state boardsData

processDataHelper : {seen: List U64, remaining: List U64}, List {board: List U64, rowscols: List (List U64)} -> U64
processDataHelper = \{seen, remaining}, boardsData ->
    when List.findFirst boardsData (\boardData -> hasBingo boardData.rowscols seen) is
        Ok boardData ->
            # Calculate the final result
            unmarkedSum =
                boardData.board
                |> List.dropIf (\val -> List.contains seen val)
                |> List.sum
            lastSeen = List.last seen |> Result.withDefault 0
            unmarkedSum * lastSeen
        Err _ ->
            when List.first remaining is
                Ok next ->
                    processDataHelper {seen: List.append seen next, remaining: List.dropFirst remaining} boardsData
                Err _ ->
                    # This should be impossible, just panic
                    0 - 1

hasBingo : List (List U64), List U64 -> Bool
hasBingo = \rowscols, seen ->
    rowscols
    |> List.map (\sublist -> List.dropIf sublist (\val -> List.contains seen val))
    |> List.keepIf List.isEmpty
    |> List.isEmpty
    |> Bool.not
