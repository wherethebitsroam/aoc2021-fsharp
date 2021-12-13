module Day12

type Cave =
    | Start
    | End
    | Big of string
    | Small of string

let parseCave s =
    match s with
    | "start" -> Start
    | "end" -> End
    | s ->
        if s.ToUpper() = s then
            Big s
        else
            Small s

let parseLink (s: string) =
    let x = s.Split('-')
    (parseCave x[0], parseCave x[1])

let addCaveToMap (m: Map<Cave,Set<Cave>>) (c1: Cave,c2: Cave) =
    m |> Map.change c1 (fun x ->
        match x with
        | Some x -> Some (Set.add c2 x)
        | None -> Some (set [ c2 ])
    )

let addCavesToMap m (c1,c2) =
    [(c1,c2); (c2,c1)] |> List.fold addCaveToMap m

let parse (s: string) =
    s.Trim().Split('\n')
    |> Seq.map parseLink
    |> Seq.fold addCavesToMap (Map [])

let rec explore m (path: Cave list) (remaining: Set<Cave>): Cave list list =
    let cave = List.head path
    m |> Map.find cave
      |> Set.intersect remaining
      |> Set.toList
      |> List.collect (fun next -> 
          match next with
          | Start -> []
          | End -> [End :: path]
          | Small _ -> explore m (next :: path) (Set.remove next remaining ) 
          | Big _ -> explore m (next :: path) remaining
      )

let part1 s =
    let m = parse s
    let possible = Map.keys m
    explore m [Start] (Set possible) |> List.length

let remaining (visited: Map<Cave,int>): Set<Cave> =
    let hasDouble = visited
                    |> Map.fold (fun s cave count ->
                        s || match cave with
                             | Small _ -> count > 1
                             | _ -> false
                        ) false
    
    visited |> Map.filter (fun k v ->
        match k with
        | Small _ -> match v with
                     | 0 -> true
                     | 1 -> not hasDouble
                     | _ -> false
        | _ -> true
    )   |> Map.keys
        |> Set

let incrCave x =
    match x with
    | Some x -> Some (x + 1)
    | None -> Some 1

let rec explore2 m (path: Cave list) (visited: Map<Cave,int>): Cave list list =
    let cave = List.head path
    m |> Map.find cave
      |> Set.intersect (remaining visited)
      |> Set.toList
      |> List.collect (fun next -> 
          match next with
          | Start -> []
          | End -> [End :: path]
          | Small _ | Big _ -> explore2 m (next :: path) (Map.change next incrCave visited) 
      )

let part2 s =
    let m = parse s
    let visted = m |> Map.map (fun k _ -> 0)
    explore2 m [Start] visted |> List.length