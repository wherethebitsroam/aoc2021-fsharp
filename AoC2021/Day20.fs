module Day20

type Image = { map: Map<int *int, int>; outside: int }

let parseAlg (s: string) =
    s |> Seq.toArray

let parseImgLine (y: int, s: string) =
    s |> Seq.toList |> Seq.indexed |> Seq.map (fun (x, c) -> if c = '#' then ((y,x),1) else (y,x),0)

let parseImg (s: string) =
    s.Split("\n")
    |> Seq.indexed
    |> Seq.collect parseImgLine
    |> Map.ofSeq

let parse (s: string) =
    let parts = s.Trim().Split("\n\n")

    let alg = parseAlg parts[0]
    if Array.length alg <> 512 then
        failwithf "Expected alg to have length 512, got %d" (Array.length alg)
    let img = parseImg parts[1]

    (alg,{ map = img; outside = 0 })

let bounds img =
    let ys = img |> Map.keys |> Seq.map fst
    let xs = img |> Map.keys |> Seq.map snd

    let ymin = ys |> Seq.min
    let ymax = ys |> Seq.max
    let xmin = xs |> Seq.min
    let xmax = xs |> Seq.max

    ((ymin,ymax),(xmin,xmax))

let renderLine img xs y =
    xs |> List.map (fun x -> if Map.tryFind (y,x) img = Some 1 then "#" else ".") |> String.concat ""

let render img =
    let ((ymin,ymax),(xmin,xmax)) = bounds img.map

    let header = sprintf "y: %d,%d, x: %d,%d, outside: %d" ymin ymax xmin xmax img.outside

    [ymin..ymax]
    |> List.map (renderLine img.map [xmin..xmax])
    |> List.append [header]
    |> String.concat "\n"

let toBinary l =
    l |> List.rev |> List.indexed |> List.map (fun (i, v) -> v <<< i)  |> List.sum

let lookup img p =
    match Map.tryFind p img.map with
    | Some x -> x
    | None -> img.outside

// get the 9-bit value centered on (y,x)
let value img (y,x) =
    List.allPairs [y - 1; y; y + 1] [x - 1; x; x + 1] 
    |> List.map (lookup img)
    |> toBinary

let update (alg: char array) img p = alg.[value img p]

let updateOutside outside c =
    match outside, c with
    | 0, '#' -> 1
    | 1, '#' -> 0
    | _, '.' -> 0
    | _ -> failwithf "bad %d %c" outside c

let step (alg: char array) (img: Image) =
    let ((ymin,ymax),(xmin,xmax)) = bounds img.map

    let map = update alg img

    let newMap =
        List.allPairs [(ymin - 1)..(ymax + 1)] [(xmin - 1)..(xmax + 1)]
        |> List.map (fun p -> if map p = '#' then (p,1) else (p,0))
        |> Map.ofList
    
    { map = newMap; outside = updateOutside img.outside alg[0] }

let rec iterate n (alg: char array) (img: Image) =
    if n = 0 then
        img
    else iterate (n - 1) alg (step alg img)

let part1 (s: string) =
    let (alg, img) = parse s

    iterate 2 alg img
    |> fun img -> img.map
    |> Map.values
    |> Seq.filter (fun v -> v = 1)
    |> Seq.length

let part2 (s: string) =
    let (alg, img) = parse s

    iterate 50 alg img
    |> fun img -> img.map
    |> Map.values
    |> Seq.filter (fun v -> v = 1)
    |> Seq.length