let data =
    System.IO.File.ReadAllText @"../../day12.txt"

// let test2 =
//     "5483143223
// 2745854711
// 5264556173
// 6141336146
// 6357385478
// 4167524645
// 2176841721
// 6882881134
// 4846848554
// 5283751526"

// let test1 =
//     "11111
// 19991
// 19191
// 19991
// 11111"

let test1 =
    "start-A
start-b
A-c
A-b
b-d
A-end
b-end"

let test2 =
    "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"

let test3 =
    "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"

printfn "%A" (Day12.part2 data)
