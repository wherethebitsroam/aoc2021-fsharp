module Day23

open System.Collections.Generic

type Amphipod =
    | A
    | B
    | C
    | D

module Amphipod =
    let cost a =
        match a with
        | A -> 1
        | B -> 10
        | C -> 100
        | D -> 1000

    let print a =
        match a with
        | A -> "A"
        | B -> "B"
        | C -> "C"
        | D -> "D"

    let printOption a =
        match a with
        | Some a -> print a
        | None -> "."

// #############
// #01.2.3.4.56# Hall
// ###B#C#B#D### T
//   #A#D#C#A#   B
//   #########
//    A B C D    Room

type Location =
    | Hall of int // 0 - 6. How to restrict?
    | RoomTop of Amphipod
    | RoomBottom of Amphipod

module Location =
    // All of the direct connections between locations along with the steps
    let connections =
        [ (Hall 0, Hall 1, 1)
          (Hall 1, Hall 2, 2)
          (Hall 2, Hall 3, 2)
          (Hall 3, Hall 4, 2)
          (Hall 4, Hall 5, 2)
          (Hall 5, Hall 6, 1)
          (RoomTop A, Hall 1, 2)
          (RoomTop A, Hall 2, 2)
          (RoomTop B, Hall 2, 2)
          (RoomTop B, Hall 3, 2)
          (RoomTop C, Hall 3, 2)
          (RoomTop C, Hall 4, 2)
          (RoomTop D, Hall 4, 2)
          (RoomTop D, Hall 5, 2)
          (RoomTop A, RoomBottom A, 1)
          (RoomTop B, RoomBottom B, 1)
          (RoomTop C, RoomBottom C, 1)
          (RoomTop D, RoomBottom D, 1) ]

    let connectionMap =
        let changeFn v l1 =
            match l1 with
            | Some l -> Some(v :: l)
            | None -> Some [ v ]

        let updateMap (l1: Location, l2: Location, c: int) m =
            m
            |> Map.change l1 (changeFn (l2, c))
            |> Map.change l2 (changeFn (l1, c))

        connections
        |> List.fold (fun m v -> updateMap v m) (Map [])

    let rec paths (path, cost) l =
        connectionMap
        |> Map.find l
        |> List.filter (fun (l2, _) -> not (List.contains l2 path))
        |> List.collect (fun (l2, c) ->
            let p = (l2 :: path, cost + c)
            p :: paths p l2)
        // only keep the cheapest
        |> List.groupBy (fun (p, _) -> List.head p)
        |> List.map (fun (_, lst) -> List.minBy (fun (_, c) -> c) lst)
        // we can't return to ourself
        |> List.filter (fun (p, _) -> List.head p <> l)

    let possiblePaths =
        connectionMap
        |> Map.map (fun l _ -> paths ([], 0) l)
        |> Map.map (fun _ paths ->
            paths
            |> List.map (fun (p, c) -> (List.head p, (p, c)))
            |> Map.ofList)

type Burrow = Map<Location, Amphipod>

module Burrow =
    let get (l: Location) (b: Burrow) = b |> Map.tryFind l

    let set (l: Location) (oa: Amphipod option) (b: Burrow) =
        match oa with
        | None -> Map.remove l b
        | Some a -> Map.add l a b

    let printLoction (b: Burrow) (l: Location) = get l b |> Amphipod.printOption

    let ofList (l: (Location * Amphipod) list) : Burrow = l |> Map.ofList

    let each (b: Burrow) = b |> Map.toList

    let move (src: Location) (dst: Location) (b: Burrow) =
        if src = dst then
            failwith "can't move to same location"

        match get src b with
        | None -> failwith "nothing in source location"
        | Some a ->
            match get dst b with
            | Some x -> failwithf "destination location is occupied by %A" x
            | None -> b |> set src None |> set dst (Some a)

    let possibleRoom (a: Amphipod) (b: Burrow) =
        match get (RoomBottom a) b, get (RoomTop a) b with
        | None, None -> [ RoomBottom a ]
        | Some a, None -> [ RoomTop a ]
        | _ -> []

    let rightSpot (l: Location) (a: Amphipod) (b: Burrow) =
        match l with
        | Hall _ -> false
        | RoomTop x -> x = a && Map.tryFind (RoomBottom x) b = Some a
        | RoomBottom x -> x = a

    let possible (l: Location, a: Amphipod) (b: Burrow) =
        if rightSpot l a b then
            []
        else
            match l with
            | Hall _ -> possibleRoom a b
            | _ ->
                possibleRoom a b
                @ [ Hall 0
                    Hall 1
                    Hall 2
                    Hall 3
                    Hall 4
                    Hall 5
                    Hall 6 ]

    let route (src: Location) (dst: Location) (b: Burrow) =
        Location.possiblePaths
        |> Map.tryFind src
        |> Option.bind (fun m ->
            m
            |> Map.tryFind dst
            |> Option.bind (fun (path, cost) ->
                if path
                   |> List.map (fun pl -> get pl b)
                   |> List.exists Option.isSome then
                    None
                else
                    Some cost))

    let next (b: Burrow) =
        each b
        |> List.collect (fun (l, a) -> possible (l, a) b |> List.map (fun d -> (l, a, d)))
        |> List.choose (fun (l, a, d) ->
            route l d b
            |> Option.map (fun cost -> (l, a, d, cost * Amphipod.cost a)))
        |> List.sortBy (fun (_, _, _, c) -> c)

    let isDone (b: Burrow) =
        each b
        |> List.forall (fun (l, a) -> rightSpot l a b)

let test =
    [ (RoomTop A, B)
      (RoomBottom A, A)
      (RoomTop B, C)
      (RoomBottom B, D)
      (RoomTop C, B)
      (RoomBottom C, C)
      (RoomTop D, D)
      (RoomBottom D, A) ]
    |> Burrow.ofList

let data =
    [ (RoomTop A, B)
      (RoomBottom A, C)
      (RoomTop B, A)
      (RoomBottom B, D)
      (RoomTop C, B)
      (RoomBottom C, D)
      (RoomTop D, C)
      (RoomBottom D, A) ]
    |> Burrow.ofList

let tryMin l =
    match l with
    | [] -> None
    | l -> Some(List.min l)

let blah (b: Burrow) =
    // printfn "depth: %d\n%s" depth (Burrow.print b)
    let seen = new Dictionary<Burrow, int option>()

    let rec blahInner (b: Burrow) =
        if Burrow.isDone b then
            Some 0
        elif seen.ContainsKey(b) then
            seen.[b]
        else
            let min =
                Burrow.next b
                |> List.choose (fun (s, _, d, c) ->
                    blahInner (Burrow.move s d b)
                    |> Option.map (fun x -> c + x))
                |> tryMin

            seen.[b] <- min

            min

    blahInner b

let part1 () = printfn "%A" (blah data)

type Location2 =
    | Hall of int // 0 - 6. How to restrict?
    | RoomTop of Amphipod
    | RoomMidTop of Amphipod
    | RoomMidBot of Amphipod
    | RoomBottom of Amphipod

module Location2 =
    // All of the direct connections between locations along with the steps
    let connections =
        [ (Hall 0, Hall 1, 1)
          (Hall 1, Hall 2, 2)
          (Hall 2, Hall 3, 2)
          (Hall 3, Hall 4, 2)
          (Hall 4, Hall 5, 2)
          (Hall 5, Hall 6, 1)
          (RoomTop A, Hall 1, 2)
          (RoomTop A, Hall 2, 2)
          (RoomTop B, Hall 2, 2)
          (RoomTop B, Hall 3, 2)
          (RoomTop C, Hall 3, 2)
          (RoomTop C, Hall 4, 2)
          (RoomTop D, Hall 4, 2)
          (RoomTop D, Hall 5, 2)
          (RoomTop A, RoomMidTop A, 1)
          (RoomTop B, RoomMidTop B, 1)
          (RoomTop C, RoomMidTop C, 1)
          (RoomTop D, RoomMidTop D, 1)
          (RoomMidTop A, RoomMidBot A, 1)
          (RoomMidTop B, RoomMidBot B, 1)
          (RoomMidTop C, RoomMidBot C, 1)
          (RoomMidTop D, RoomMidBot D, 1)
          (RoomMidBot A, RoomBottom A, 1)
          (RoomMidBot B, RoomBottom B, 1)
          (RoomMidBot C, RoomBottom C, 1)
          (RoomMidBot D, RoomBottom D, 1) ]

    let connectionMap =
        let changeFn v l1 =
            match l1 with
            | Some l -> Some(v :: l)
            | None -> Some [ v ]

        let updateMap (l1: Location2, l2: Location2, c: int) m =
            m
            |> Map.change l1 (changeFn (l2, c))
            |> Map.change l2 (changeFn (l1, c))

        connections
        |> List.fold (fun m v -> updateMap v m) (Map [])

    let rec paths (path, cost) l =
        connectionMap
        |> Map.find l
        |> List.filter (fun (l2, _) -> not (List.contains l2 path))
        |> List.collect (fun (l2, c) ->
            let p = (l2 :: path, cost + c)
            p :: paths p l2)
        // only keep the cheapest
        |> List.groupBy (fun (p, _) -> List.head p)
        |> List.map (fun (_, lst) -> List.minBy (fun (_, c) -> c) lst)
        // we can't return to ourself
        |> List.filter (fun (p, _) -> List.head p <> l)

    let possiblePaths =
        connectionMap
        |> Map.map (fun l _ -> paths ([], 0) l)
        |> Map.map (fun _ paths ->
            paths
            |> List.map (fun (p, c) -> (List.head p, (p, c)))
            |> Map.ofList)

type Burrow2 = Map<Location2, Amphipod>

module Burrow2 =
    let get (l: Location2) (b: Burrow2) = b |> Map.tryFind l

    let set (l: Location2) (oa: Amphipod option) (b: Burrow2) =
        match oa with
        | None -> Map.remove l b
        | Some a -> Map.add l a b

    let printLoction (b: Burrow2) (l: Location2) = get l b |> Amphipod.printOption

    let ofList (l: (Location2 * Amphipod) list) : Burrow2 = l |> Map.ofList

    let each (b: Burrow2) = b |> Map.toList

    let move (src: Location2) (dst: Location2) (b: Burrow2) =
        if src = dst then
            failwith "can't move to same location"

        match get src b with
        | None -> failwith "nothing in source location"
        | Some a ->
            match get dst b with
            | Some x -> failwithf "destination location is occupied by %A" x
            | None -> b |> set src None |> set dst (Some a)

    let possibleRoom (a: Amphipod) (b: Burrow2) =
        match get (RoomBottom a) b, get (RoomMidBot a) b, get (RoomMidTop a) b, get (RoomTop a) b with
        | None, None, None, None -> [ RoomBottom a ]
        | Some x, None, None, None when x = a -> [ RoomMidBot a ]
        | Some x, Some y, None, None when x = a && y = a -> [ RoomMidTop a ]
        | Some x, Some y, Some z, None when x = a && y = a && z = a -> [ RoomTop a ]
        | _ -> []

    let rightSpot (l: Location2) (a: Amphipod) (b: Burrow2) =
        match l with
        | Hall _ -> false
        | RoomTop x ->
            x = a
            && get (RoomBottom x) b = Some a
            && get (RoomMidBot x) b = Some a
            && get (RoomMidTop x) b = Some a
        | RoomMidTop x ->
            x = a
            && get (RoomBottom x) b = Some a
            && get (RoomMidBot x) b = Some a
        | RoomMidBot x -> x = a && get (RoomBottom x) b = Some a
        | RoomBottom x -> x = a

    let possible (l: Location2, a: Amphipod) (b: Burrow2) =
        if rightSpot l a b then
            []
        else
            match l with
            | Hall _ -> possibleRoom a b
            | _ ->
                possibleRoom a b
                @ [ Hall 0
                    Hall 1
                    Hall 2
                    Hall 3
                    Hall 4
                    Hall 5
                    Hall 6 ]

    let route (src: Location2) (dst: Location2) (b: Burrow2) =
        Location2.possiblePaths
        |> Map.tryFind src
        |> Option.bind (fun m ->
            m
            |> Map.tryFind dst
            |> Option.bind (fun (path, cost) ->
                if path
                   |> List.map (fun pl -> get pl b)
                   |> List.exists Option.isSome then
                    None
                else
                    Some cost))

    let next (b: Burrow2) =
        each b
        |> List.collect (fun (l, a) -> possible (l, a) b |> List.map (fun d -> (l, a, d)))
        |> List.choose (fun (l, a, d) ->
            route l d b
            |> Option.map (fun cost -> (l, a, d, cost * Amphipod.cost a)))
        |> List.sortBy (fun (_, _, _, c) -> c)

    let isDone (b: Burrow2) =
        each b
        |> List.forall (fun (l, a) -> rightSpot l a b)

let test2 =
    [ (RoomTop A, B)
      (RoomTop B, C)
      (RoomTop C, B)
      (RoomTop D, D)
      (RoomMidTop A, D)
      (RoomMidTop B, C)
      (RoomMidTop C, B)
      (RoomMidTop D, A)
      (RoomMidBot A, D)
      (RoomMidBot B, B)
      (RoomMidBot C, A)
      (RoomMidBot D, C)
      (RoomBottom A, A)
      (RoomBottom B, D)
      (RoomBottom C, C)
      (RoomBottom D, A) ]
    |> Burrow2.ofList

let data2 =
    [ (RoomTop A, B)
      (RoomTop B, A)
      (RoomTop C, B)
      (RoomTop D, C)
      (RoomMidTop A, D)
      (RoomMidTop B, C)
      (RoomMidTop C, B)
      (RoomMidTop D, A)
      (RoomMidBot A, D)
      (RoomMidBot B, B)
      (RoomMidBot C, A)
      (RoomMidBot D, C)
      (RoomBottom A, C)
      (RoomBottom B, D)
      (RoomBottom C, D)
      (RoomBottom D, A) ]
    |> Burrow2.ofList

let blah2 (b: Burrow2) =
    // printfn "depth: %d\n%s" depth (Burrow.print b)
    let seen = new Dictionary<Burrow2, int option>()

    let rec blah2Inner (b: Burrow2) =
        if Burrow2.isDone b then
            Some 0
        elif seen.ContainsKey(b) then
            seen.[b]
        else
            let min =
                Burrow2.next b
                |> List.choose (fun (s, _, d, c) ->
                    blah2Inner (Burrow2.move s d b)
                    |> Option.map (fun x -> c + x))
                |> tryMin

            seen.[b] <- min

            min

    blah2Inner b

let part2 () = printfn "%A" (blah2 data2)
