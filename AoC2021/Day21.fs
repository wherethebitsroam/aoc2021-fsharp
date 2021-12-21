module Day21

type Dice() =
    let mutable next = 0
    let mutable count = 0

    member _.Roll =
        count <- count + 1
        next <- (next % 100) + 1
        next

    member _.Count = count

type Player = { pos: int; score: int }

let rec play (dice: Dice) (p1: Player) (p2: Player) =
    let rolls =
        [ dice.Roll; dice.Roll; dice.Roll ] |> List.sum

    let pos = (p1.pos + rolls - 1) % 10 + 1
    let score = p1.score + pos
    let p1 = { pos = pos; score = score }

    if score >= 1000 then
        (p1, p2)
    else
        play dice p2 p1

let part1 pos1 pos2 =
    let p1 = { pos = pos1; score = 0 }
    let p2 = { pos = pos2; score = 0 }
    let dice = Dice()

    let (_, loser) = play dice p1 p2

    loser.score * dice.Count

// get all the possible totals for 3 dice rolls, along with the counts of their occurance
let possible =
    [ 1 .. 3 ]
    |> List.collect (fun r1 ->
        [ 1 .. 3 ]
        |> List.collect (fun r2 ->
            [ 1 .. 3 ]
            |> List.collect (fun r3 -> [ r1 + r2 + r3 ])))
    |> List.countBy id
    |> List.map (fun (r, c) -> (r, int64 c))

let folder (m: Map<int, int64>) (p, c) =
    m
    |> Map.change p (fun x ->
        match x with
        | Some x -> Some(x + c)
        | None -> Some(c))

let rec play2 (player: int) (chances: int64) (p1: Player) (p2: Player) =
    // kick off a game for each possible roll
    possible
    |> List.collect (fun (roll, count) ->
        let pos = (p1.pos + roll - 1) % 10 + 1
        let score = p1.score + pos
        let p1 = { pos = pos; score = score }

        if score >= 21 then
            [ (player, chances * count) ]
        else
            // switch players and go again
            let player = if player = 1 then 2 else 1
            play2 player (chances * count) p2 p1)
    // collect all of the outcomes into a count for each player
    |> List.fold folder (Map [])
    |> Map.toList

let part2 pos1 pos2 =
    let p1 = { pos = pos1; score = 0 }
    let p2 = { pos = pos2; score = 0 }

    play2 1 1 p1 p2
