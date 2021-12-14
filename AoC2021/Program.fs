let data =
    System.IO.File.ReadAllText @"../../day14.txt"

let test1 =
    "11111
19991
19191
19991
11111"

let test2 =
    "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"

let test =
    "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

printfn "%A" (Day14.part2 data)
