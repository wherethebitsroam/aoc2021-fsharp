module Day25

type SeaCucumber =
    | E
    | S

type SeaBed =
    { loc: Map<int * int, SeaCucumber>
      w: int
      h: int }

module SeaBed =
    let east (x, y) sb = ((x + 1) % sb.w, y)
    let south (x, y) sb = (x, (y + 1) % sb.h)

    let move (herd: SeaCucumber) (sb: SeaBed) =
        let loc =
            sb.loc
            |> Map.toList
            |> List.map (fun (l, sc) ->
                if sc = herd then
                    let next =
                        match sc with
                        | E -> east l sb
                        | S -> south l sb

                    match Map.tryFind next sb.loc with
                    | None -> (next, sc)
                    | _ -> (l, sc)
                else
                    (l, sc))
            |> Map.ofList

        { sb with loc = loc }


    let step sb = sb |> move E |> move S

    let parseLine (y: int, s: string) =
        s
        |> Seq.indexed
        |> Seq.choose (fun (x, c) ->
            match c with
            | 'v' -> Some((x, y), S)
            | '>' -> Some((x, y), E)
            | _ -> None)
        |> Seq.toList

    let parse (s: string) =
        let lines = s.Trim().Split("\n") |> Seq.toList

        let loc =
            lines
            |> Seq.indexed
            |> Seq.map parseLine
            |> Seq.toList

        let h = List.length loc
        let w = lines |> List.head |> String.length

        { loc = (loc |> List.collect id |> Map.ofList)
          w = w
          h = h }

    let print sb =
        [ 0 .. (sb.h - 1) ]
        |> List.map (fun y ->
            [ 0 .. (sb.w - 1) ]
            |> List.map (fun x ->
                match Map.tryFind (x, y) sb.loc with
                | Some E -> ">"
                | Some S -> "v"
                | None -> ".")
            |> String.concat "")
        |> String.concat "\n"

let run sb =
    let mutable count = 0
    let mutable x = true
    let mutable sb = sb

    while x do
        let nsb = SeaBed.step sb
        count <- count + 1

        if nsb = sb then
            printfn "Done! Count=%d" count
            x <- false

        sb <- nsb

let part1 (s: string) =
    let sb = SeaBed.parse s

    // printfn "%s" (SeaBed.print sb)
    run sb
