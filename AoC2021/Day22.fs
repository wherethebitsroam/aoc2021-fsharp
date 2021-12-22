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

type Range = int * int

module Range =
    let overlap (a1, a2) (b1, b2) =
        if (a1 >= b1 && a1 <= b2) || (b1 >= a1 && b1 <= a2) then
            Some(max a1 b1, min a2 b2)
        else
            None

    let toList (a, b) = [ (max a -50) .. (min b 50) ]

    // x=30018..52561
    let parse (s: string) =
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

    let len (a, b) = int64 (b - a + 1)

type Cube = { x: Range; y: Range; z: Range }

module Cube =
    // x=30018..52561,y=6820..20726,z=63544..81739
    let parse (s: string) =
        match s.Split(",") with
        | [| x; y; z |] ->
            { x = Range.parse x
              y = Range.parse y
              z = Range.parse z }
        | _ -> failwith "bad cube"

    let points s =
        Range.toList s.x
        |> List.collect (fun x ->
            Range.toList s.y
            |> List.collect (fun y -> Range.toList s.z |> List.map (fun z -> (x, y, z))))

    let overlap c1 c2 =
        Range.overlap c1.x c2.x
        |> Option.bind (fun x ->
            Range.overlap c1.y c2.y
            |> Option.bind (fun y ->
                Range.overlap c1.z c2.z
                |> Option.map (fun z -> { x = x; y = y; z = z })))

    let size c =
        Range.len c.x * Range.len c.y * Range.len c.z

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
    Cube.overlap step.cube prev.cube
    |> Option.map (fun overlap ->
        { onOff = OnOff.toggle prev.onOff
          cube = overlap })

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
