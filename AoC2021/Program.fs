let data =
    System.IO.File.ReadAllText @"../../day16.txt"

printfn "%A" (Day16.part2 data)
