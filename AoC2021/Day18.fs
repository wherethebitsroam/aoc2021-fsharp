module Day18

type Pair = Element * Element

and Element =
    | Int of int
    | Pair of Pair

module Pair =
    let rec print (p: Pair) : string =
        let (e1, e2) = p

        [ "["
          printElement e1
          ","
          printElement e2
          "]" ]
        |> String.concat ""

    and printElement (e: Element) : string =
        match e with
        | Int i -> i |> string
        | Pair p -> print p


let expect exp cs =
    match cs with
    | [] -> failwithf "Expected %c, got empty" exp
    | c :: cs ->
        if c <> exp then
            failwithf "Expected %c, got %c" exp c

        cs

let rec parseElement cs : Element * char list =
    match cs with
    | [] -> failwithf "Expected SnailItem, got empty"
    | c :: cs ->
        match c with
        | '[' ->
            let (sf, cs) = parsePair (c :: cs)
            (Pair sf, cs)
        | i -> (i.ToString() |> int |> Int, cs)

and parsePair cs : Pair * char list =
    let cs = expect '[' cs
    let (l, cs) = parseElement cs
    let cs = expect ',' cs
    let (r, cs) = parseElement cs
    let cs = expect ']' cs
    ((l, r), cs)

let parseLine (s: string) : Pair = s |> Seq.toList |> parsePair |> fst

let add (s1: Pair) (s2: Pair) : Pair = (Pair s1, Pair s2)

type Explosion<'a> =
    | Exploded of 'a * left: int option * right: int option
    | Unexploded of 'a

module Explosion =
    let map f x =
        match x with
        | Exploded (x, l, r) -> Exploded(f x, l, r)
        | Unexploded x -> Unexploded(f x)

let rec addRight (p: Pair) (r: int) =
    match p with
    | (Int i, elem) -> (Int(i + r), elem)
    | (Pair p, elem) -> (Pair(addRight p r), elem)

let rec addLeft (p: Pair) (l: int) =
    match p with
    | (elem, Int i) -> (elem, Int(i + l))
    | (elem, Pair p) -> (elem, Pair(addLeft p l))

let rec explodeElement (depth: int) (e: Element) =
    match e with
    | Pair p ->
        if depth = 4 then
            match p with
            | (Int l, Int r) -> Exploded(Int 0, Some l, Some r)
            | p -> failwithf "Didn't expect %A" p
        else
            explodePair depth p |> Explosion.map Pair
    | Int i -> Unexploded e

and explodePair (depth: int) (p: Pair) : Explosion<Pair> =
    match explodeElement (depth + 1) (fst p) with
    | Exploded (e1, l, r) ->
        match r, snd p with
        | Some r, Int i -> Exploded((e1, Int(i + r)), l, None)
        | Some r, Pair p2 -> Exploded((e1, Pair(addRight p2 r)), l, None)
        | None, elem -> Exploded((e1, elem), l, r)
    | Unexploded e1 ->
        match explodeElement (depth + 1) (snd p) with
        | Exploded (e2, l, r) ->
            match l, e1 with
            | Some l, Int i -> Exploded((Int(i + l), e2), None, r)
            | Some l, Pair p1 -> Exploded((Pair(addLeft p1 l), e2), None, r)
            | None, elem -> Exploded((elem, e2), l, r)
        | Unexploded e2 -> Unexploded(e1, e2)

let explode (p: Pair) = explodePair 0 p

type Split<'a> =
    | Split of 'a
    | NotSplit of 'a

module Split =
    let map f x =
        match x with
        | Split x -> Split(f x)
        | NotSplit x -> NotSplit(f x)

let rec splitElement (e: Element) : Split<Element> =
    match e with
    | Int i ->
        if i > 9 then
            let l = i / 2
            let r = i - l
            Split(Pair(Int l, Int r))
        else
            NotSplit(Int i)
    | Pair p -> split p |> Split.map Pair

and split (p: Pair) : Split<Pair> =
    let (e1, e2) = p

    match splitElement e1 with
    | Split e1 -> Split(e1, e2)
    | NotSplit e1 ->
        match splitElement e2 with
        | Split e2 -> Split(e1, e2)
        | NotSplit e2 -> NotSplit(e1, e2)


let rec reduce (p: Pair) =
    match explode p with
    | Exploded (p, _, _) -> reduce p
    | Unexploded p ->
        match split p with
        | Split p -> reduce p
        | NotSplit p -> p

let add_reduce (p1: Pair) (p2: Pair) = add p1 p2 |> reduce

let parse (s: string) =
    s.Trim().Split("\n")
    |> Seq.map parseLine
    |> Seq.toList

let sum (s: string) =
    match parse s with
    | [] -> failwith "pairs empty"
    | p :: ps -> List.fold add_reduce p ps

let rec elementMagnitude (e: Element) =
    match e with
    | Int i -> i
    | Pair p -> magnitude p

and magnitude (e1: Element, e2: Element) =
    3 * elementMagnitude e1 + 2 * elementMagnitude e2

let part1 (s: string) = sum s |> magnitude

let rec comb ps =
    match ps with
    | [] -> []
    | p :: ps ->
        (List.collect (fun x -> [ (p, x); (x, p) ]) ps)
        @ comb ps

let part2 (s: string) =
    parse s
    |> comb
    |> List.map (fun (a, b) -> add_reduce a b)
    |> List.map magnitude
    |> List.max
