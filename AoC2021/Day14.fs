module Day14

let toTuple (s: string) =
    let l = Seq.toArray s
    (l.[0], l.[1])

// parses a pair to it's expansion
// CH -> B => (('C','H'),[('C','B'),('B','H')]
let parsePair (s: string) =
    let x = s.Split(" -> ")
    let src = toTuple x.[0]
    let insert = Seq.head x.[1]
    (src, [ (fst src, insert); (insert, snd src) ])

let groupSum counts =
    counts
    |> List.groupBy fst
    |> List.map (fun (x, l) -> (x, l |> List.map snd |> List.sum))

let parse (s: string) =
    let x = s.Trim().Split("\n\n")

    let chars = x.[0] |> Seq.toList
    let h = List.head chars
    let t = List.last chars

    // create the expansion map
    let m =
        x.[1].Split('\n')
        |> Seq.map parsePair
        |> Map.ofSeq

    // group input by pairs and count each pair
    let counts =
        chars
        |> List.pairwise
        |> List.map (fun pair -> (pair, 1L))
        |> groupSum

    (h, t, counts, m)

let expand m (pair, count) =
    let expansion = Map.find pair m
    expansion |> List.map (fun pair -> (pair, count))

// map each pair to their expansion, keeping the count, they re-group
let step m pairs =
    pairs |> List.collect (expand m) |> groupSum

// since all characters are pairwise, we will have double
// of all chars except for the first and last
let counts (h: char) (t: char) (pairs: list<(char * char) * int64>) =
    let count =
        pairs
        |> List.collect (fun (pair, count) -> [ (fst pair, count); (snd pair, count) ])

    // add an extra copy of the head and tail so everything is double
    // then divide the count by 2
    count @ [ (h, 1); (t, 1) ]
    |> groupSum
    |> List.map (fun (x, count) -> (x, count / 2L))


let part1 (s: string) =
    let (h, t, pairs, map) = parse s
    let mutable res = pairs

    for _ in 0 .. 9 do
        res <- step map res

    let count = counts h t res |> List.map snd
    let max = count |> List.max
    let min = count |> List.min

    max - min

let part2 (s: string) =
    let (h, t, pairs, map) = parse s
    let mutable res = pairs

    for _ in 0 .. 39 do
        res <- step map res

    let count = counts h t res |> List.map snd
    let max = count |> List.max
    let min = count |> List.min

    max - min
