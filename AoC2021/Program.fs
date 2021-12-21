let data =
    System.IO.File.ReadAllText @"../../day19.txt"

let test =
    System.IO.File.ReadAllText @"../../day19-test.txt"

// real
// Player 1 starting position: 3
// Player 2 starting position: 5

// test
// Player 1 starting position: 4
// Player 2 starting position: 8

printfn "%A" (Day21.part2 3 5)
