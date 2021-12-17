module Bytecode

type Reader = { bytes: byte []; byte: int; bit: int }

let reader (bytes: byte []) = { bytes = bytes; byte = 0; bit = 8 }

let mask from len =
    ((1 <<< len) - 1) <<< (from - len) |> byte

let readBits (b: byte) from len = (b &&& mask from len) >>> (from - len)

let readPartial (n: int) (r: Reader) =
    if n > r.bit then
        failwithf "trying to read %d when bit is %d" n r.bit

    let res = readBits r.bytes.[r.byte] r.bit n |> int

    let (bit, byt) =
        match r.bit - n with
        | 0 -> (8, r.byte + 1)
        | x -> (x, r.byte)

    (res,
     { bytes = r.bytes
       byte = byt
       bit = bit })

let read (n: int) (r: Reader) =
    let mutable rdr = r
    let mutable n = n
    let mutable res = 0

    while n > 0 do
        let rn = [ rdr.bit; n ] |> List.min
        let (x, r) = readPartial rn rdr
        res <- res ||| (x <<< (n - rn))
        n <- n - rn
        rdr <- r

    (res, rdr)

type Op =
    | Sum
    | Product
    | Min
    | Max
    | Gt
    | Lt
    | Eq

type Packet =
    { version: int
      value: Value
      len: int }

and Value =
    | Literal of int64
    | Operator of Op * Packet list


let readLiteral (r: Reader) =
    let mutable res = 0L
    let mutable run = true
    let mutable r = r
    let mutable n = 0

    while run do
        let (x, rdr) = read 5 r
        res <- (res <<< 4) ||| (x &&& 0b1111)
        r <- rdr
        n <- n + 5
        if x &&& 0b10000 = 0 then run <- false

    let litSize = n * 4 / 5

    if litSize > 63 then
        failwithf "literal overflow: %d bits" litSize

    (Literal res, n, r)

let lengthCond n packets =
    packets |> List.map (fun p -> p.len) |> List.sum < n

let countCond n packets = packets |> List.length < n

let rec readOperator op (r: Reader) =
    let (lengthType, r) = read 1 r
    let mutable rn = 1

    let (cond, r) =
        match lengthType with
        | 0 ->
            let (length, r) = read 15 r
            rn <- rn + 15
            (lengthCond length, r)
        | _ ->
            let (count, r) = read 11 r
            rn <- rn + 11
            (countCond count, r)

    let mutable packets = []
    let mutable r = r

    while cond packets do
        let (p, rdr) = readPacket r
        rn <- rn + p.len
        packets <- packets @ [ p ]
        r <- rdr

    (Operator(op, packets), rn, r)


and readPacket (r: Reader) =
    let (version, r) = read 3 r
    let (typ, r) = read 3 r

    let (value, n, r) =
        match typ with
        | 0 -> readOperator Sum r
        | 1 -> readOperator Product r
        | 2 -> readOperator Min r
        | 3 -> readOperator Max r
        | 4 -> readLiteral r
        | 5 -> readOperator Gt r
        | 6 -> readOperator Lt r
        | 7 -> readOperator Eq r
        | x -> failwithf "bad type: %d" x

    ({ version = version
       value = value
       len = 6 + n },
     r)
