module Day19

type Point = { x: int; y: int; z: int }

// module Point =
//     let shifts (p: Point) = [p, {x=p.y; y=}]

let parsePoint (s: string) =
    let a = s.Split(",") |> Array.map int
    { x = a.[0]; y = a.[1]; z = a.[2] }

let parseScanner (s: string) =
    match s.Split("\n") |> Seq.toList with
    | [] -> failwith "missing list"
    | h :: rest -> rest |> List.map parsePoint |> List.toArray

let parse (s: string) =
    s.Trim().Split("\n\n") |> Array.map parseScanner
