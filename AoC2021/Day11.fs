module Day11

type Point = { x: int; y: int }

let add p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }

let valid l p =
    p.x >= 0 && p.x < l && p.y >= 0 && p.y < l

let side vals =
    let l = List.length vals

    match l with
    | 100 -> 10
    | 25 -> 5
    | _ -> failwithf "arrgghh"

let parseLine (y, s: string) =
    Seq.toList s
    |> Seq.map (fun c -> c.ToString())
    |> Seq.map int
    |> Seq.indexed
    |> Seq.map (fun (x, v) -> ({ x = x; y = y }, v))
    |> Seq.toList

let parse (s: string) =
    s.Trim().Split('\n')
    |> Seq.indexed
    |> Seq.collect parseLine
    |> Seq.toList

let deltas =
    [ { x = 1; y = 1 }
      { x = 1; y = 0 }
      { x = 1; y = -1 }
      { x = 0; y = 1 }
      { x = 0; y = -1 }
      { x = -1; y = 1 }
      { x = -1; y = 0 }
      { x = -1; y = -1 } ]

let neigbours l p =
    deltas
    |> List.map (fun d -> add p d)
    |> List.filter (fun p -> valid l p)

let groupSum counts =
    counts
    |> List.groupBy fst
    |> List.map (fun (k, l) -> (k, l |> List.map snd |> List.sum))

let rec flash (flashed: Set<Point>) vals =
    let flashers =
        vals
        |> List.filter (fun (_, v) -> v > 9)
        |> List.filter (fun (p, _) -> not (Set.contains p flashed))
        |> List.map fst

    let neigbours = neigbours (side vals)

    match flashers with
    | [] ->
        // reset the flashers
        let x =
            vals
            |> List.map (fun (p, c) -> (p, (if c > 9 then 0 else c)))

        (Set.count flashed, x)
    | l ->
        let flashed =
            l |> List.fold (fun s v -> Set.add v s) flashed

        // a neighbour in ((y,x), 1) then sum based on the same (x,y)
        let extra =
            l
            |> List.collect neigbours
            |> List.map (fun x -> (x, 1))

        let vals = extra @ vals |> groupSum
        flash flashed vals


let step x =
    x
    |> List.map (fun (p, v) -> (p, v + 1))
    |> flash (Set [])

let render vals =
    let l = side vals

    for y in 0 .. (l - 1) do
        for x in 0 .. (l - 1) do
            let b = { x = x; y = y }
            let (_, c) = vals |> List.find (fun (p, _) -> p = b)
            printf "%d" c

        printf "\n"


let part1 (data: string) =
    let mutable vals = parse data
    let mutable f = 0

    for _ in 1 .. 100 do
        let (flashed, v) = step vals
        vals <- v
        f <- f + flashed

    f

let part2 (s: string) =
    let mutable vals = parse s
    let mutable stp = 0
    let l = List.length vals

    for i in 1 .. 1000 do
        if stp = 0 then
            let (flashed, v) = step vals
            vals <- v
            if flashed = l then stp <- i

    stp
