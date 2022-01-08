let data =
    System.IO.File.ReadAllText @"../../day24.txt"

let test =
    "inp w
mul x 0
add x z
mod x 26
div z 26
add x -14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 13
mul y x
add z y"

// printfn "%A" (Day23.part1 data)
printfn "%A" (Day24.part1 data)
