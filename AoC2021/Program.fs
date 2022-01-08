let data =
    System.IO.File.ReadAllText @"../../day25.txt"

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

printfn "%A" (Day25.part1 data)
