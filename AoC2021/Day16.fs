module Day16

open System

// http://www.fssnip.net/gf/title/Hex-string-to-byte-array
let fromHex (s: string) =
    s
    |> Seq.windowed 2
    |> Seq.mapi (fun i j -> (i, j))
    |> Seq.filter (fun (i, j) -> i % 2 = 0)
    |> Seq.map (fun (_, j) -> Byte.Parse(new System.String(j), System.Globalization.NumberStyles.AllowHexSpecifier))
    |> Array.ofSeq

let rec sumVersion (packet: Bytecode.Packet) =
    match packet.value with
    | Bytecode.Literal _ -> packet.version
    | Bytecode.Operator (_, ps) ->
        packet.version
        + (ps |> List.map sumVersion |> List.sum)

let pairEval l op =
    if List.length l <> 2 then
        failwith "pair eval with len != 2"

    let a = l |> List.toArray
    if op a.[0] a.[1] then 1 else 0

let rec eval (packet: Bytecode.Packet) =
    match packet.value with
    | Bytecode.Literal x -> int64 x
    | Bytecode.Operator (op, ps) ->
        let evaled = ps |> List.map eval

        match op with
        | Bytecode.Sum -> evaled |> List.sum
        | Bytecode.Product -> evaled |> List.fold (*) 1
        | Bytecode.Min -> evaled |> List.min
        | Bytecode.Max -> evaled |> List.max
        | Bytecode.Gt -> pairEval evaled (>)
        | Bytecode.Lt -> pairEval evaled (<)
        | Bytecode.Eq -> pairEval evaled (=)

let rec dump level (packet: Bytecode.Packet) =
    let spaces =
        Array.create level " " |> String.concat ""

    let value =
        match packet.value with
        | Bytecode.Literal x -> x.ToString()
        | Bytecode.Operator (op, ps) ->
            let dumped =
                ps
                |> List.map (dump (level + 1))
                |> String.concat ",\n"

            let ops =
                match op with
                | Bytecode.Sum -> "sum"
                | Bytecode.Product -> "product"
                | Bytecode.Min -> "min"
                | Bytecode.Max -> "max"
                | Bytecode.Gt -> "gt"
                | Bytecode.Lt -> "lt"
                | Bytecode.Eq -> "eq"

            [ ops
              "("
              "\n"
              dumped
              "\n"
              spaces
              ")" ]
            |> String.concat ""

    [ spaces; value ] |> String.concat ""

let part1 (s: string) =
    let bytes = fromHex (s.Trim())
    let r = Bytecode.reader bytes
    let (packet, _) = Bytecode.readPacket r
    sumVersion packet

let part2 (s: string) =
    let bytes = fromHex (s.Trim())
    let r = Bytecode.reader bytes
    let (packet, _) = Bytecode.readPacket r
    // printfn "%s" (dump 0 packet)
    eval packet
