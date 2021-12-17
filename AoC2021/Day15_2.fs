module Day15_2

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


let neighbours map (y, x) =
    let l = Array2D.length1 map

    [ (y, x - 1)
      (y, x + 1)
      (y - 1, x)
      (y + 1, x) ]
    |> List.filter (fun (y, x) -> x >= 0 && x < l && y >= 0 && y < l)
    |> List.toSeq

let gCost (map: int [,]) _ (y, x) = map.[y, x] |> float

let fCost (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1) |> float

let navigate map =
    let l = Array2D.length1 map

    AStar.search
        (0, 0)
        (l - 1, l - 1)
        { neighbours = neighbours map
          gCost = gCost map
          fCost = fCost
          maxIterations = None }

let pathCost map path =
    match path with
    | None -> failwith "wtf"
    | Some path ->
        path
        |> Seq.filter (fun p -> p <> (0, 0))
        |> Seq.map (gCost map (0, 0))
        |> Seq.toList
        |> List.sum

let part1 (s: string) =
    let map = parse s
    navigate map |> pathCost map

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

    navigate map |> pathCost map
