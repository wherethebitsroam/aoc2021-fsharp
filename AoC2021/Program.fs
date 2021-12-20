let data =
    System.IO.File.ReadAllText @"../../day19.txt"

let test =
    System.IO.File.ReadAllText @"../../day19-test.txt"

printfn "%A" (Day19.part2 data)
