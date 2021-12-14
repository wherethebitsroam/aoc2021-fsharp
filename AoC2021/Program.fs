let data =
    System.IO.File.ReadAllText @"../../day11.txt"

printfn "%A" (Day11.part2 data)
