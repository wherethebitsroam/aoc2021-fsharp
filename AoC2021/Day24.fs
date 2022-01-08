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
        | _ -> failwithf "unknown variable %s" s

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
    let fn op =
        match op with
        | Add -> fun x y -> x + y
        | Mul -> fun x y -> x * y
        | Div -> fun x y -> x / y
        | Mod -> fun x y -> x % y
        | Eql -> fun x y -> if x = y then 1 else 0

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
    // An Op between 2 Calcs in the calc table
    | Calc of Op * int * int
    // A ref to another table value
    | Ref of int

module Calc =
    let print c =
        match c with
        | Idx i -> sprintf "[%d]" i
        | Val v -> sprintf "%d" v
        | Ref r -> sprintf "@%d" r
        | Calc (op, l, r) ->
            match op with
            | Add -> sprintf "calc @%d + @%d" l r
            | Mul -> sprintf "calc @%d * @%d" l r
            | Div -> sprintf "calc @%d / @%d" l r
            | Mod -> sprintf "calc @%d %% @%d" l r
            | Eql -> sprintf "calc @%d = @%d" l r

type ALU =
    { vars: Map<Variable, int>
      idx: int
      calcs: Calc array
      calcIdx: int }

module ALU =
    let create =
        { vars = [ (W, 0); (X, 0); (Y, 0); (Z, 0) ] |> Map.ofList
          idx = 0
          calcs = [| Val 0 |] // all vars initially have a zero value
          calcIdx = 1 }

    let apply inst alu =
        match inst with
        | Inp v ->
            let calcs =
                Array.append alu.calcs [| (Idx alu.idx) |]

            let vars = Map.add v alu.calcIdx alu.vars

            { vars = vars
              idx = alu.idx + 1
              calcs = calcs
              calcIdx = alu.calcIdx + 1 }
        | Op (op, v, arg) ->
            match arg with
            | Variable va ->
                let calc =
                    Calc(op, Map.find v alu.vars, Map.find va alu.vars)

                let calcs = Array.append alu.calcs [| calc |]

                let vars = Map.add v alu.calcIdx alu.vars

                { alu with
                    calcs = calcs
                    vars = vars
                    calcIdx = alu.calcIdx + 1 }
            | Number x ->
                let calc =
                    Calc(op, Map.find v alu.vars, alu.calcIdx)

                let calcs =
                    Array.append alu.calcs [| (Val x); calc |]

                let vars = Map.add v (alu.calcIdx + 1) alu.vars

                { alu with
                    calcs = calcs
                    vars = vars
                    calcIdx = alu.calcIdx + 2 }

    let simplify alu =
        let calcs = alu.calcs

        calcs
        |> Array.indexed
        |> Array.iter (fun (i, c) ->
            match c with
            | Idx _ -> ()
            | Val _ -> ()
            | Ref _ -> ()
            | Calc (op, l, r) ->
                match op with
                | Mul ->
                    match calcs.[l], calcs.[r] with
                    | Val 0, _
                    | _, Val 0 -> calcs.[i] <- Val 0
                    | Val 1, Calc _ -> calcs.[i] <- Ref r
                    | Calc _, Val 1 -> calcs.[i] <- Ref l
                    | Val 1, x
                    | x, Val 1 -> calcs.[i] <- x
                    | Val v1, Val v2 -> calcs.[i] <- Val(v1 * v2)
                    | _ -> ()
                | Add ->
                    match calcs.[l], calcs.[r] with
                    | Val 0, Calc _ -> calcs.[i] <- Ref r
                    | Calc _, Val 0 -> calcs.[i] <- Ref l
                    | Val 0, x
                    | x, Val 0 -> calcs.[i] <- x
                    | Val v1, Val v2 -> calcs.[i] <- Val(v1 + v2)
                    | _ -> ()
                | Div ->
                    match calcs.[l], calcs.[r] with
                    | Calc _, Val 1 -> calcs.[i] <- Ref l
                    | x, Val 1 -> calcs.[i] <- x
                    | Val v1, Val v2 -> calcs.[i] <- Val(v1 / v2)
                    | _ -> ()
                | Mod ->
                    match calcs.[l], calcs.[r] with
                    | Val v1, Val v2 -> calcs.[i] <- Val(v1 % v2)
                    | _ -> ()
                | Eql ->
                    match calcs.[l], calcs.[r] with
                    | Val v1, Val v2 -> calcs.[i] <- Val(if v1 = v2 then 1 else 0)
                    | _ -> ())

        alu

    let parse (s: string) =
        s.Trim().Split("\n")
        |> Seq.map Instruction.parse
        |> Seq.fold (fun alu inst -> apply inst alu) create

    let print alu =
        printfn "%A" alu.vars

        alu.calcs
        |> Array.indexed
        |> Array.iter (fun (i, calc) -> printfn "%03d: %s" i (Calc.print calc))

    let eval (arr: int array) alu =
        let out = Array.create (Array.length alu.calcs) 0

        alu.calcs
        |> Array.indexed
        |> Array.iter (fun (i, c) ->
            match c with
            | Idx idx -> out.[i] <- arr.[idx]
            | Val v -> out.[i] <- v
            | Ref r -> out.[i] <- out.[r]
            | Calc (op, l, r) -> out.[i] <- Op.fn op out.[l] out.[r])

        out.[Map.find Z alu.vars]


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

// So, evaluating in fsharp was too slow and I couldn't find a good
// way to "solve" it, so I thought I would export to AssemblyScript,
// compile to wasm and it would be much faster. It was, but not fast
// enough. So I refactored to:
//
// export function day24calc(arr: Int8Array): i64 {
//   var z: i64 = 0
//
//   z = arr[0] + 12
//
//   z = z * 26 + arr[1] + 7
//
//   z = z * 26 + arr[2] + 8
//
//   z = z * 26 + arr[3] + 8
//
//   if (arr[4] - 1 != arr[5]) {
//     z = z * 26 + arr[5] + 12
//   }
//   if (z % 26 + 10 != arr[6]) {
//     z = z * 26 + arr[6] + 8
//   }
//   if (z % 26 - 11 == arr[7]) {
//     z = z / 26
//   } else {
//     z = (z / 26) * 26 + arr[7] + 13
//   }
//   if (z % 26 - 13 == arr[8]) {
//     z = z / 26
//   } else {
//     z = (z / 26) * 26 + arr[8] + 3
//   }
//   if (z % 26 + 13 != arr[9]) {
//     z = z * 26 + arr[9] + 13
//   }
//   if (z % 26 - 8 == arr[10]) {
//     z = z / 26
//   } else {
//     z = (z / 26) * 26 + arr[10] + 3
//   }
//   if (z % 26 - 1 == arr[11]) {
//     z = z / 26 >> 0
//   } else {
//     z = (z / 26 >> 0) * 26 + arr[11] + 9
//   }
//   if (z % 26 - 4 == arr[12]) {
//     z = z / 26
//   } else {
//     z = (z / 26) * 26 + arr[12] + 4
//   }
//   if (z % 26 - 14 == arr[13]) {
//     z = z / 26
//   } else {
//     z = (z / 26) * 26 + arr[13] + 13
//   }
//   return z
// }
//
// It was faster, but still not fast enough (~10million numbers per second)
// But I realised that it was sort of making base 26 numbers
// and shifting them left and right, which led to:
//
// export function day24calc2(arr: Int8Array): i64 {
//   var b26 = new Int8Array(14);
//
//   b26[0] = arr[0] + 12
//   b26[1] = arr[1] + 7
//   b26[2] = arr[2] + 8
//   b26[3] = arr[3] + 8
//   var cur = 3
//
//   if (arr[4] - 1 != arr[5]) {
//     cur++
//     b26[cur] = arr[5] + 12
//   }
//
//   cur++
//   b26[cur] = arr[6] + 8
//
//   if (arr[6] - 3 == arr[7]) {
//     cur--
//   } else {
//     b26[cur] = arr[7] + 13
//   }
//
//   if (b26[cur] - 13 == arr[8]) {
//     cur--
//   } else {
//     b26[cur] = arr[8] + 3
//   }
//
//   cur++
//   b26[cur] = arr[9] + 13
//
//   if (arr[9] + 5 == arr[10]) {
//     cur--
//   } else {
//     b26[cur] = arr[10] + 3
//   }
//
//   if (b26[cur] - 1 == arr[11]) {
//     cur--
//   } else {
//     b26[cur] = arr[11] + 9
//   }
//
//   if (b26[cur] - 4 == arr[12]) {
//     cur--
//   } else {
//     b26[cur] = arr[12] + 4
//   }
//
//   if (b26[cur] - 14 == arr[13]) {
//     cur--
//   } else {
//     b26[cur] = arr[13] + 13
//   }
//
//   var z: i64 = 0
//   for (var i = 0; i <= cur; i++) {
//     z = z * 26 + b26[i]
//   }
//
//   return z
// }
//
// Which was about 2 x slower. BUT I realised that
// since everything assigned to b26 was a +ve integer,
// the only way the result could be 0 was if the number
// of right shift equalled the number of left shifts.
// The only way this was possible is if the first if
// statement was false and all of the rest were true.
// Following this logic led to these rules for getting 0:
//
// arr[5] = arr[4] - 1
// arr[7] = arr[6] - 3
// arr[8] = arr[3] - 5
// arr[10] = arr[9] + 5
// arr[11] = arr[2] + 7
// arr[12] = arr[1] + 3
// arr[13] = arr[0] - 2
//
// Which I then used for making the biggest and smallest number

module AS =
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


let part1 (s: string) =
    let test =
        numToList 13579246899999L |> Array.ofList

    let alu = ALU.parse s |> ALU.simplify
    printfn "ALU: %d" (ALU.eval test alu)
    printfn "Simple: %d" (Simple.eval s test)

    AS.export s
