module Day10

// let test =
//     "[({(<(())[]>[[{[]{<()<>>\n\
//     [(()[<>])]({[<{<<[]>>(\n\
//     {([(<{}[<>[]}>{[]{[(<()>\n\
//     (((({<>}<{<{<>}{[]{[]{}\n\
//     [[<[([]))<([[{}[[()]]]\n\
//     [{[{({}]{}}([{[{{{}}([]\n\
//     {<[[]]>}<{[{[{[]{()[[[]\n\
//     [<(<(<(<{}))><([]([]()\n\
//     <{([([[(<>()){}]>(<<{{\n\
//     <{([{{}}[<[[[<>{}]]]>[]]"

// let lines = test.Split '\n'

type Validation =
    | Ok
    | Incomplete of char list
    | Invalid of char

let takeInvalid v =
    match v with
    | Invalid c -> Some c
    | _ -> None

let takeIncomplete v =
    match v with
    | Incomplete s -> Some s
    | _ -> None

let points (c: char) =
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | c -> failwithf "invalid char %c" c

let closer c =
    match c with
    | '<' -> '>'
    | '(' -> ')'
    | '{' -> '}'
    | '[' -> ']'
    | c -> failwithf "invalid char %c" c

let closerPoints c =
    match c with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | c -> failwithf "invalid char %c" c

let rec matchChar stack chars =
    match chars with
    | [] ->
        match stack with
        | [] -> Ok
        | s -> Incomplete s
    | head :: tail ->
        // if the current character is an opener, add it
        // to the stack and process the remaining
        match head with
        | '<'
        | '('
        | '{'
        | '[' as s -> matchChar (s :: stack) tail
        | h ->
            match stack with
            | [] -> Invalid h
            | s :: stack ->
                if h = closer s then
                    matchChar (stack) tail
                else
                    Invalid h

let autocomplete stack = stack |> List.map closer

let autocompletePoints stack =
    stack
    |> List.rev
    |> List.map closerPoints
    |> List.indexed
    |> List.map (fun (i, p) -> (pown 5L i) * p)
    |> List.sum

let parseLine (line: string) =
    let chars = Seq.toList line
    matchChar [] chars

let part1 (data: string) =
    data.Split '\n'
    |> Seq.map parseLine
    |> Seq.choose takeInvalid
    |> Seq.map points
    |> Seq.sum

let inline median input =
    let sorted = input |> Seq.toArray |> Array.sort
    let middle = (sorted.Length - 1) / 2
    sorted.[middle]

let part2 (data: string) =
    data.Split '\n'
    |> Seq.map parseLine
    |> Seq.choose takeIncomplete
    |> Seq.map autocomplete
    |> Seq.map autocompletePoints
    |> Seq.toList
    |> median
