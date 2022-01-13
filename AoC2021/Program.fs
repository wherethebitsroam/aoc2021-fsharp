let data =
    System.IO.File.ReadAllText @"../../day24-are.txt"

let test =
    "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"

printfn "%A" (Day24.part1 data)
