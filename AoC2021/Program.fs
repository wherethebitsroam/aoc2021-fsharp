let data =
    System.IO.File.ReadAllText @"../../day16.txt"

printfn "%A" (Day17.part2 data)
