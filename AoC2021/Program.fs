let data =
    System.IO.File.ReadAllText @"../../day18.txt"

printfn "%A" (Day18.part2 data)
