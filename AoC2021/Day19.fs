module Day19

type Vector = int array
type Matrix = int array array

module Vector =
    let cross op v1 v2 =
        Array.zip v1 v2
        |> Array.map (fun (a, b) -> op a b)
        |> Array.sum

    let mul = cross (*)

    let apply op v1 v2 =
        Array.zip v1 v2
        |> Array.map (fun (a, b) -> op a b)

    let add = apply (+)
    let sub = apply (-)

    let transform (tf: Matrix) (v: Vector) : Vector = tf |> Array.map (fun r -> mul r v)

    let manhattan v1 v2 =
        Array.zip v1 v2
        |> Array.map (fun (a, b) -> abs (a - b))
        |> Array.sum

module Matrix =
    let mul (m1: Matrix) (m2: Matrix) =
        let m2 = Array.transpose m2

        m1
        |> Array.map (fun r -> m2 |> Array.map (fun c -> Vector.mul r c))

    let muls (ms: Matrix list) =
        match ms with
        | [] -> failwithf "empty list"
        | m :: ms -> ms |> List.fold mul m

module Transforms =
    let identity =
        [| [| 1; 0; 0 |]
           [| 0; 1; 0 |]
           [| 0; 0; 1 |] |]

    let rotateX =
        [| [| 1; 0; 0 |]
           [| 0; 0; -1 |]
           [| 0; 1; 0 |] |]

    let rotateY =
        [| [| 0; 0; 1 |]
           [| 0; 1; 0 |]
           [| -1; 0; 0 |] |]

    let rotateZ =
        [| [| 0; -1; 0 |]
           [| 1; 0; 0 |]
           [| 0; 0; 1 |] |]

    let rotations tf =
        [ identity
          tf
          Matrix.muls [ tf; tf ]
          Matrix.muls [ tf; tf; tf ] ]

    let all =
        let xs = rotations rotateX
        let ys = rotations rotateY
        let zs = rotations rotateZ

        xs
        |> List.collect (fun x ->
            ys
            |> List.collect (fun y -> zs |> List.map (fun z -> Matrix.muls [ x; y; z ])))
        |> List.distinct

let parsePoint (s: string) = s.Split(",") |> Array.map int

let parseScanner (s: string) =
    match s.Split("\n") |> Seq.toList with
    | [] -> failwith "missing list"
    | h :: rest -> rest |> List.map parsePoint

let parse (s: string) =
    s.Trim().Split("\n\n") |> Array.map parseScanner

let findOverlaps (avs: Vector list) (bvs: Vector list) =
    avs
    |> List.allPairs bvs
    |> List.map (fun (a, b) -> Vector.sub b a)
    |> List.groupBy id
    |> List.map (fun (v, l) -> (v, List.length l))
    |> List.sortByDescending snd
    |> List.head

let applyTranform (vs: Vector list) (tf: Matrix) = vs |> List.map (Vector.transform tf)

let tryPermutations (avs: Vector list) (bvs: Vector list) =
    Transforms.all
    |> List.map (applyTranform bvs)
    |> List.choose (fun transformed ->
        match findOverlaps avs transformed with
        | (location, 12) -> Some(location, transformed)
        | _ -> None)
    |> List.tryHead

let rec comb ps =
    match ps with
    | [] -> []
    | p :: ps -> (List.map (fun x -> (p, x)) ps) @ comb ps

type Scanner =
    { location: Vector
      beacons: Vector list }

let rec reduce (s: int) (scanners: int array list array) (discovered: Map<int, Scanner>) =
    let x = Map.find s discovered

    // get all of the undiscovered scanners with their id
    let remaining =
        scanners
        |> Array.indexed
        |> Array.choose (fun (i, s) ->
            if Map.containsKey i discovered then
                None
            else
                Some(i, s))

    if Array.isEmpty remaining then
        discovered
    else
        let matched =
            remaining
            |> Array.choose (fun (i, bs) ->
                match tryPermutations x.beacons bs with
                | Some (location, beacons) ->
                    Some(
                        i,
                        { location = location
                          beacons = beacons |> List.map (Vector.add location) }
                    )
                | None -> None)

        // add the match values to the discovered map
        let discovered =
            matched
            |> Array.fold (fun m (i, scanner) -> Map.add i scanner m) discovered

        // reduce further
        matched
        |> Array.map fst
        |> Array.fold (fun m i -> reduce i scanners m) discovered

let locateAll (s: string) =
    let scanners = parse s

    let discovered =
        [ (0,
           { location = [| 0; 0; 0 |]
             beacons = scanners.[0] }) ]
        |> Map.ofList

    reduce 0 scanners discovered

let part1 (s: string) =
    locateAll s
    |> Map.values
    |> Seq.collect (fun s -> s.beacons)
    |> Seq.distinct
    |> Seq.length


let part2 (s: string) =
    locateAll s
    |> Map.values
    |> Seq.map (fun a -> a.location)
    |> Seq.toList
    |> comb
    |> List.map (fun (a, b) -> Vector.manhattan a b)
    |> List.max
