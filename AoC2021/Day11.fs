module Day11

let toInts (s: string) =
    Seq.toList s
    |> Seq.map (fun c -> c.ToString())
    |> Seq.map int
    |> Seq.toArray

// flashers get set to 0 immediately
let incr i = if i = 9 then 0 else i + 1

let incrCells cells = cells |> Array2D.map incr

let step cells = cells |> incrCells

let flash cells = cells |> Array2D.mapi

let part1 (data: string) =
    let cellArray = data.Split '\n' |> Array.map toInts
    let l = Array.length cellArray

    let cells =
        Array2D.init l l (fun i j -> cellArray.[i].[j])

    step cells
