module Day15

let parseLine (s: string) =
    s
    |> Seq.toList
    |> Seq.map (fun c -> int (c.ToString()))
    |> Seq.toList

let parse (s: string) : int [,] =
    s.Trim().Split('\n')
    |> Seq.map parseLine
    |> Seq.toList
    |> array2D


let rec optimize map (scores: int [,]) (y, x) =
    let l = Array2D.length1 map

    let neighbors =
        [ (y, x - 1)
          (y, x + 1)
          (y - 1, x)
          (y + 1, x) ]
        |> List.filter (fun (y, x) -> x >= 0 && x < l && y >= 0 && y < l)

    let min =
        neighbors
        |> List.map (fun (y, x) -> scores.[y, x])
        |> List.min

    let score = min + map.[y, x]

    if score < scores.[y, x] then
        scores.[y, x] <- score

let navigate map =
    let l = Array2D.length1 map
    let scores = Array2D.create l l 0

    for y in 0 .. (l - 1) do
        for x in 0 .. (l - 1) do
            let options =
                [ (y, x - 1); (y - 1, x) ]
                |> List.filter (fun (y, x) -> x >= 0 && y >= 0)
                |> List.map (fun (y, x) -> scores.[y, x])

            scores.[y, x] <-
                match options with
                | [] -> 0
                | options -> (List.min options) + map.[y, x]

    // hackety hack
    for _ in 1 .. 20 do
        for y in 0 .. (l - 1) do
            for x in 0 .. (l - 1) do
                optimize map scores (y, x)

    scores.[(l - 1), (l - 1)]

let part1 (s: string) = parse s |> navigate

let add x delta =
    let xx = x + delta
    if xx > 9 then xx - 9 else xx

let part2 (s: string) =
    let orig = parse s
    let l = Array2D.length1 orig

    let map = Array2D.create (l * 5) (l * 5) 0

    for y in 0 .. (l - 1) do
        for x in 0 .. (l - 1) do
            for dy in 0 .. 4 do
                for dx in 0 .. 4 do
                    map.[y + dy * l, x + dx * l] <- add orig.[y, x] (dy + dx)

    navigate map
