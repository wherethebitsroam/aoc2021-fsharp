module Day13

type Fold = X of int | Y of int

type Point = {x: int; y: int}

let parseFoldDir s =
    match s with
    | "x" -> X
    | "y" -> Y
    | c -> failwithf "Invalid fold: %s" c

let parsePoint (s: string) =
    let bits = s.Split(',')
    {x = int bits[0]; y = int bits[1]}

let parseFold (s: string) = 
    // e.g. fold along y=7
    let bits = s.Split(' ')
    let b2 = bits[2].Split('=')
    let fold = parseFoldDir b2[0]

    fold (int b2[1])

let parse (s: string) =
    let s = s.Split("\n\n")
    let points = s[0].Split('\n') |> Seq.map parsePoint
    let folds = s[1].Trim().Split('\n') |> Seq.map parseFold

    (points, folds)

let foldPoint f p =
    match f with
    | X fx ->
        match p with
        | {x = px; y = py} when px > fx -> {x = 2*fx - px; y = py}
        | p -> p
    | Y fy ->
        match p with
        | {x = px; y = py} when py > fy -> {x = px; y = 2*fy - py}
        | p -> p

let fold points f =
    points
    |> Seq.map (foldPoint f)
    |> Seq.distinct

let part1 (s: string) =
    let (points, folds) = parse s
    let f = Seq.head folds

    Seq.length (fold points f)

let render points =
    let xmax = points |> Seq.map (fun p -> p.x) |> Seq.max
    let ymax = points |> Seq.map (fun p -> p.y) |> Seq.max

    for y in 0..ymax do
        for x in 0..xmax do
            if Seq.contains {x=x;y=y} points then
                printf "#"
            else
                printf "."
        printf "\n"

let part2 (s: string) =
    let (points, folds) = parse s

    folds |> Seq.fold fold points |> render
