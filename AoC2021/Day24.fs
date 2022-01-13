module Day24

type Variable =
    | W
    | X
    | Y
    | Z

module Variable =
    let parse (s: string) =
        match s with
        | "w" -> W
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | _ -> failwithf "unknown variable '%s'" s

    let var v =
        match v with
        | W -> "w"
        | X -> "x"
        | Y -> "y"
        | Z -> "z"

type Arg =
    | Variable of Variable
    | Number of int

module Arg =
    let parse (s: string) =
        match s with
        | "w" -> Variable W
        | "x" -> Variable X
        | "y" -> Variable Y
        | "z" -> Variable Z
        | n -> Number(int n)

type Op =
    | Add
    | Mul
    | Div
    | Mod
    | Eql

module Op =
    let fn64 op =
        match op with
        | Add -> fun x y -> x + y
        | Mul -> fun x y -> x * y
        | Div -> fun x y -> x / y
        | Mod -> fun x y -> x % y
        | Eql -> fun x y -> if x = y then 1L else 0L

    let parse (s: string) =
        match s with
        | "add" -> Add
        | "mul" -> Mul
        | "div" -> Div
        | "mod" -> Mod
        | "eql" -> Eql
        | _ -> failwithf "unknown op %s" s

type Instruction =
    | Inp of Variable
    | Op of Op * Variable * Arg

module Instruction =
    let parse (s: string) =
        match s.Split(" ") with
        | [| "inp"; v |] -> Inp(Variable.parse v)
        | [| op; v; a |] -> Op(Op.parse op, Variable.parse v, Arg.parse a)
        | _ -> failwithf "unknown instruction %s" s

type Calc =
    // an index into the provided array
    | Idx of int
    // an explicit value
    | Val of int
    // a variable
    | Var of int
    // An Op between 2 Calcs in the calc map
    | Calc of Op * Calc * Calc

module Calc =
    let rec print c =
        match c with
        | Idx i -> sprintf "[%d]" i
        | Val v -> sprintf "%d" v
        | Var v -> sprintf "x%d" v
        | Calc (op, l, r) ->
            match op with
            | Add -> sprintf "%s + %s" (print l) (print r)
            | Mul -> sprintf "%s * %s" (print l) (print r)
            | Div -> sprintf "%s / %s" (print l) (print r)
            | Mod -> sprintf "%s %% %s" (print l) (print r)
            | Eql -> sprintf "%s = %s" (print l) (print r)

type ALU = { output: int; calcs: Map<int, Calc> }

module ALU =
    let parse (s: string) =
        let mutable vars =
            [ (W, 0); (X, 0); (Y, 0); (Z, 0) ] |> Map.ofList

        let mutable next = 1
        let mutable idx = 0

        let initial = [ (0, Val 0) ] |> Map.ofList

        let folder calcs inst =
            match inst with
            | Inp v ->
                let calcs = Map.add next (Idx idx) calcs

                vars <- Map.add v next vars
                idx <- idx + 1
                next <- next + 1

                calcs
            | Op (op, v, arg) ->
                let vver = (Map.find v vars)

                let a =
                    match arg with
                    | Variable a ->
                        let asub = (Map.find a vars)
                        Var asub
                    | Number x -> Val x

                let calcs =
                    Map.add next (Calc(op, Var vver, a)) calcs

                vars <- Map.add v next vars
                next <- next + 1

                calcs

        let calcs =
            s.Trim().Split("\n")
            |> Seq.map Instruction.parse
            |> Seq.fold folder initial

        { calcs = calcs
          output = Map.find Z vars }


    // makes a Set of all variables referenced in calculations
    let referenced alu =
        let referenced =
            alu.calcs
            |> Map.values
            |> Seq.toList
            |> List.collect (fun c ->
                match c with
                | Idx _ -> []
                | Val _ -> []
                | Var v -> [ v ]
                | Calc (_, v1, v2) ->
                    match v1, v2 with
                    | Var v1, Var v2 -> [ v1; v2 ]
                    | _, Var v2 -> [ v2 ]
                    | Var v1, _ -> [ v1 ]
                    | _ -> [])

        alu.output :: referenced |> Set.ofList

    let removeUnreferenced alu =
        let refs = referenced alu

        let calcs =
            Map.filter (fun k _ -> Set.contains k refs) alu.calcs

        { alu with calcs = calcs }

    let substitute alu =
        let subs =
            alu.calcs
            |> Map.filter (fun _ c ->
                match c with
                | Calc _ -> false
                | _ -> true)

        let sub c =
            match c with
            | Var v ->
                match Map.tryFind v subs with
                | Some x -> x
                | _ -> c
            | _ -> c

        let calcs =
            alu.calcs
            |> Map.map (fun v c ->
                match c with
                | Calc (op, l, r) ->
                    let l = sub l
                    let r = sub r
                    Calc(op, l, r)
                | Var _ -> sub c
                | _ -> c)

        { alu with calcs = calcs }

    let minMax alu =
        let min (x, _) = x
        let max (_, x) = x

        let rec calcMinMax c m =
            match c with
            | Idx _ -> (1L, 9L)
            | Val x -> (x, x)
            | Var v -> Map.find v m
            | Calc (op, l, r) ->
                let l = calcMinMax l m
                let r = calcMinMax r m

                match op with
                | Add -> (min l + min r, max l + max r)
                | Mul -> (min l * min r, max l * max r)
                | Div -> (min l / max r, max l / min r)
                | Mod -> (0, [ max l; max r ] |> List.min)
                | Eql -> (0, 1)

        // FIXME ordering!!
        alu.calcs
        |> Map.fold
            (fun m v c ->
                let mm = calcMinMax c m
                Map.add v mm m)
            (Map [])
        |> Map.find alu.output

    let extract vv alu =
        let mutable take = true

        // FIXME ordering!!
        let calcs =
            alu.calcs
            |> Map.filter (fun v _ ->
                if v = vv then
                    take <- false
                    true
                else
                    take)

        { output = vv; calcs = calcs }

    let find v alu = alu.calcs |> Map.find v

    let simplify alu =
        let calcs =
            alu.calcs
            |> Map.map (fun v c ->
                match c with
                | Calc (op, l, r) ->
                    match op with
                    | Add ->
                        match l, r with
                        | Val l, Val r -> Val(l + r)
                        | Val 0, x
                        | x, Val 0 -> x
                        // ((x + a) + b) = x + (a+b)
                        | Var var, Val v1
                        | Val v1, Var var ->
                            match find var alu with
                            | Calc (Add, x, Val v2)
                            | Calc (Add, Val v2, x) -> Calc(Add, x, Val(v1 + v2))
                            | _ -> c
                        | _ -> c
                    | Mul ->
                        match l, r with
                        | Val 0, _
                        | _, Val 0 -> Val 0
                        | Val 1, x
                        | x, Val 1 -> x
                        | _ -> c
                    | Div ->
                        match l, r with
                        | Val 0, _ -> Val 0
                        | x, Val 1 -> x
                        // ((x * b) + y) / b = x
                        | Var v, Val b1 ->
                            match find v alu with
                            | Calc (Add, Var v1, _) ->
                                match find v1 alu with
                                | Calc (Mul, x, Val b2) when b2 = b1 -> x
                                | _ -> c
                            | _ -> c
                        | _ -> c
                    | Mod ->
                        match l, r with
                        | Val 0, _ -> Val 0
                        | Var v, Val b1 ->
                            match find v alu with
                            // ((x * b) + y) % b = y
                            | Calc (Add, Var v1, y) ->
                                match find v1 alu with
                                | Calc (Mul, _, Val b2) when b2 = b1 -> y
                                | _ -> c
                            // (idx + x) % b = idx + x (when x + 9 <= b)
                            | Calc (Add, Idx i, Val x) when x + 9 <= b1 -> Calc(Add, Idx i, Val x)
                            | _ -> c
                        | _ -> c
                    | Eql ->
                        match l, r with
                        | Val l, Val r -> Val(if l = r then 1 else 0)
                        | Val v, Idx _
                        | Idx _, Val v when v < 1 || v > 9 -> Val 0
                        | Var v, Idx _ ->
                            let mm = extract v alu |> minMax

                            if fst mm > 9 || snd mm < 1 then
                                Val 0
                            else
                                c
                        | _ -> c
                | _ -> c)

        { alu with calcs = calcs }

    let reduce alu =
        let mutable count = 0
        let mutable alu = alu

        while Map.count alu.calcs <> count do
            count <- Map.count alu.calcs

            alu <-
                alu
                |> simplify
                |> substitute
                |> removeUnreferenced

        alu

    let rec removeBranching path alu =
        let findNextEql alu =
            alu.calcs
            |> Map.keys
            |> Seq.sort
            |> Seq.tryFind (fun v ->
                match Map.find v alu.calcs with
                | Calc (Eql, _, _) -> true
                | _ -> false)

        let contains x (a, b) = a <= x && x <= b

        match findNextEql alu with
        | Some vv ->
            // try 1 and 0
            [ 0; 1 ]
            |> List.collect (fun eqlVal ->
                let calcs = alu.calcs |> Map.add vv (Val eqlVal)

                let newAlu =
                    { calcs = calcs; output = alu.output } |> reduce

                if contains 0L (minMax newAlu) then
                    // possible solution
                    let ifStmt = extract vv alu |> reduce

                    removeBranching (path @ [ (ifStmt, eqlVal) ]) newAlu
                else
                    [])
        | None -> [ path ]

    let print alu =
        printfn "%A" alu.output

        alu.calcs
        |> Map.keys
        |> Seq.sort
        |> Seq.iter (fun v -> printfn "x%d = %s" v (Calc.print (Map.find v alu.calcs)))

        printfn "count: %d" (Map.count alu.calcs)

let rec numToList (num: int64) =
    if num < 10 then
        [ int num ]
    else
        numToList (num / 10L) @ [ int (num % 10L) ]

module Simple =
    let eval (s: string) (arr: int array) : int64 =
        let mutable res =
            [ (W, 0L); (X, 0L); (Y, 0L); (Z, 0L) ]
            |> Map.ofList

        let mutable idx = 0

        let set v (x: int64) = res <- Map.add v x res
        let get v = Map.find v

        s.Trim().Split("\n")
        |> Seq.map Instruction.parse
        |> Seq.iter (fun inst ->
            match inst with
            | Inp v ->
                set v arr.[idx]
                idx <- idx + 1
            | Op (op, v, arg) ->
                let r =
                    match arg with
                    | Variable va -> get va res
                    | Number n -> n

                set v (Op.fn64 op (get v res) r)

        )

        Map.find Z res

module AssemblyScript =
    let export (s: string) =
        let mutable idx = 0

        s.Trim().Split("\n")
        |> Seq.map Instruction.parse
        |> Seq.iter (fun inst ->
            match inst with
            | Inp v ->
                printfn "%s = arr[%d]" (Variable.var v) idx
                idx <- idx + 1
            | Op (op, v, arg) ->
                let r =
                    match arg with
                    | Variable va -> Variable.var va
                    | Number n -> string n

                let v = Variable.var v

                match op with
                | Add -> printfn "%s = %s + %s" v v r
                | Mul -> printfn "%s = %s * %s" v v r
                | Div -> printfn "%s = %s / %s" v v r
                | Mod -> printfn "%s = %s %% %s" v v r
                | Eql -> printfn "%s = %s == %s ? 1 : 0" v v r)

type MinMax =
    | Min
    | Max

let result (mm: MinMax) rules =
    let max (a: int array) (i1: int) (i2: int) (x: int) =
        if x > 0 then
            a.[i1] <- 9
            a.[i2] <- 9 - x
        else
            a.[i1] <- 9 + x
            a.[i2] <- 9

        a

    let min (a: int array) (i1: int) (i2: int) (x: int) =
        if x > 0 then
            a.[i1] <- 1 + x
            a.[i2] <- 1
        else
            a.[i1] <- 1
            a.[i2] <- 1 - x

        a

    let folder (a: int array) (alu: ALU, expected: int) =
        if expected <> 1 then
            failwith "expected 1"

        // x240 = [0] + -2
        // x241 = x240 = [13]
        match ALU.find alu.output alu with
        | Calc (Eql, Var v, Idx i1) ->
            match ALU.find v alu with
            | Calc (Add, Idx i2, Val x) ->
                match mm with
                | Min -> min a i1 i2 x
                | Max -> max a i1 i2 x
            | _ -> failwith "expected add"
        | _ -> failwith "expected calc"

    rules |> List.fold folder (Array.create 14 0)

let part1 (s: string) =
    let alu = ALU.parse s |> ALU.reduce

    // ALU.print alu
    // printfn "min/max: %A" (ALU.minMax alu)

    // let blah = ALU.extract (X, 35) alu |> ALU.reduce
    // ALU.print blah
    // printfn "min/max: %A" (ALU.minMax blah)

    let rules =
        alu |> ALU.removeBranching [] |> List.head

    printfn
        "%s"
        (result Max rules
         |> Array.map string
         |> String.concat "")

    printfn
        "%s"
        (result Min rules
         |> Array.map string
         |> String.concat "")
