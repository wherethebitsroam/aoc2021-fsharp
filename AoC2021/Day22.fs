module Day22

type OnOff =
    | On
    | Off

module OnOff =
    let parse (s: string) =
        match s with
        | "on" -> On
        | "off" -> Off
        | x -> failwithf "bad: %s" x

    let toggle onOff =
        match onOff with
        | On -> Off
        | Off -> On

type Cube =
    { x: int * int
      y: int * int
      z: int * int }

module Cube =
    // x=30018..52561,y=6820..20726,z=63544..81739
    let parse (s: string) =
        // x=30018..52561
        let parseRange (s: string) =
            match s.Split("=") with
            | [| _; s |] ->
                match s.Split("..") with
                | [| a; b |] ->
                    // sort the range to make things easier later
                    let a = int a
                    let b = int b
                    if b > a then (a, b) else (b, a)
                | x -> failwithf "bad range: %A" x
            | x -> failwithf "bad range: %A" x

        match s.Split(",") with
        | [| x; y; z |] ->
            { x = parseRange x
              y = parseRange y
              z = parseRange z }
        | _ -> failwith "bad cube"

    let points s =
        let range (a, b) =
            let a = max a -50
            let b = min b 50
            [ a .. b ]

        range s.x
        |> List.collect (fun x ->
            range s.y
            |> List.collect (fun y -> range s.z |> List.map (fun z -> (x, y, z))))

    let overlap c1 c2 =
        let overlap1D (a1, a2) (b1, b2) =
            if a1 >= b1 && a1 <= b2 then
                if a2 >= b1 && a2 <= b2 then
                    Some(a1, a2)
                else
                    Some(a1, b2)
            elif a2 >= b1 && a2 <= b2 then
                Some(b1, a2)
            elif b1 >= a1 && b1 <= a2 then
                Some(b1, b2)
            else
                None

        match (overlap1D c1.x c2.x, overlap1D c1.y c2.y, overlap1D c1.z c2.z) with
        | (Some x, Some y, Some z) -> Some { x = x; y = y; z = z }
        | _ -> None

    let size c =
        let len (a, b) = int64 (b - a + 1)
        len c.x * len c.y * len c.z

type Step = { onOff: OnOff; cube: Cube }

module Step =
    let parse (s: string) =
        match s.Split(" ") with
        | [| oo; rest |] ->
            { onOff = OnOff.parse oo
              cube = Cube.parse rest }

        | _ -> failwith "bad step"

    let size s =
        match s.onOff with
        | On -> Cube.size s.cube
        | Off -> -(Cube.size s.cube)

// on x=30018..52561,y=6820..20726,z=63544..81739
// off x=2437..10501,y=-80142..-77409,z=1982..17665
let parse (s: string) =
    s.Trim().Split("\n") |> Seq.map Step.parse

let updateSet onOff set point =
    match onOff with
    | On -> Set.add point set
    | Off -> Set.remove point set

let applyStep set step =
    Cube.points step.cube
    |> List.fold (updateSet step.onOff) set

let part1 (s: string) =
    parse s
    |> Seq.fold applyStep (set [])
    |> Set.count

let overlap (step: Step) (prev: Step) =
    match Cube.overlap step.cube prev.cube with
    | Some (overlap) ->
        Some
            { onOff = OnOff.toggle prev.onOff
              cube = overlap }
    | None -> None

let updates steps step =
    // make steps from where this step overlaps with previous steps
    let overlaps = steps |> List.choose (overlap step)

    // if this step is an On step, add to the list
    let updates =
        match step.onOff with
        | On -> overlaps @ [ step ]
        | Off -> overlaps

    // add to the previous steps
    steps @ updates

let part2 (s: string) =
    parse s
    |> Seq.fold updates []
    |> List.map Step.size
    |> List.sum
