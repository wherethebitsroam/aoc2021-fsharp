module Day17

let cum n = n * (n + 1) / 2

let finalY steps initial = (initial * steps) - cum (steps - 1)

let finalX steps initial =
    if steps > initial then
        cum initial
    else
        cum initial - cum (initial - steps)

let rec maxHeight prev steps initial =
    let f = finalY steps initial

    if f < prev then
        prev
    else
        maxHeight f (steps + 1) initial


let part1 (s: string) =
    let (ymin, ymax) = (-102, -78)
    // let (ymin, ymax) = (-10, -5)
    let mutable max = 0

    for i in 0 .. 1000 do
        let mutable s = 1

        while finalY s i > ymax do
            s <- s + 1

        let f = finalY s i
        if f >= ymin then max <- i

    maxHeight 0 1 max


let part2 (s: string) =
    let (ymin, ymax) = (-102, -78)
    let (xmin, xmax) = (135, 155)
    // let (ymin, ymax) = (-10, -5)
    // let (xmin, xmax) = (20, 30)
    let mutable possible = []

    for y in -102 .. 101 do
        let mutable s = 1

        while finalY s y > ymax do
            s <- s + 1

        while finalY s y >= ymin do
            for x in 0 .. 155 do
                let fx = finalX s x

                if fx >= xmin && fx <= xmax then
                    possible <- (x, y) :: possible

            s <- s + 1

    possible |> List.distinct |> List.length
