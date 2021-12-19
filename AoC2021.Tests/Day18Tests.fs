module Day18Tests

open Xunit
open Day18

[<Fact>]
let ``parse1`` () =
    let exp = (Int 1, Int 2)

    Assert.Equal(exp, parseLine "[1,2]")

[<Fact>]
let ``parse2`` () =
    let exp = (Pair(Int 1, Int 2), Int 3)

    Assert.Equal(exp, parseLine "[[1,2],3]")

[<Fact>]
let ``parse3`` () =
    let exp = (Pair(Int 1, Int 9), Pair(Int 8, Int 5))

    Assert.Equal(exp, parseLine "[[1,9],[8,5]]")

[<Fact>]
let ``split1`` () =
    let input = (Int 1, Int 9)
    let exp = NotSplit(Int 1, Int 9)

    Assert.Equal(exp, split input)

[<Fact>]
let ``split2`` () =
    let input = (Int 11, Int 9)
    let exp = Split(Pair(Int 5, Int 6), Int 9)

    Assert.Equal(exp, split input)

[<Fact>]
let ``split3`` () =
    let input = (Int 3, Int 13)
    let exp = Split(Int 3, Pair(Int 6, Int 7))

    Assert.Equal(exp, split input)

[<Fact>]
let ``split4`` () =
    let input = (Int 15, Int 13)
    let exp = Split(Pair(Int 7, Int 8), Int 13)

    Assert.Equal(exp, split input)

[<Fact>]
let ``explode1`` () =
    let input = parseLine "[[1,2],3]"
    let exp = Unexploded(input)

    Assert.Equal(exp, explode input)

[<Fact>]
let ``explode2`` () =
    let input = parseLine "[[[[[9,8],1],2],3],4]"
    let res = parseLine "[[[[0,9],2],3],4]"
    let exp = Exploded(res, Some 9, None)

    Assert.Equal(exp, explode input)

[<Fact>]
let ``explode3`` () =
    let input = parseLine "[7,[6,[5,[4,[3,2]]]]]"
    let res = parseLine "[7,[6,[5,[7,0]]]]"
    let exp = Exploded(res, None, Some 2)

    Assert.Equal(exp, explode input)

[<Fact>]
let ``explode4`` () =
    let input =
        parseLine "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"

    let res =
        parseLine "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"

    let exp = Exploded(res, None, None)

    Assert.Equal(exp, explode input)

[<Fact>]
let ``explode5`` () =
    let input =
        parseLine "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"

    let res =
        parseLine "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"

    let exp = Exploded(res, None, Some 2)

    Assert.Equal(exp, explode input)

[<Fact>]
let ``explode6`` () =
    let input =
        parseLine "[[3,[2,[8,0]]],[[[[3,4],4],1],0]]"

    let res =
        parseLine "[[3,[2,[8,3]]],[[[0,8],1],0]]"

    let exp = Exploded(res, None, None)

    Assert.Equal(exp, explode input)

[<Fact>]
let ``add test`` () =
    let i1 = parseLine "[1,2]"
    let i2 = parseLine "[[3,4],5]"

    let exp = parseLine "[[1,2],[[3,4],5]]"

    Assert.Equal(exp, add i1 i2)

[<Fact>]
let ``add reduce`` () =
    let i1 =
        parseLine "[[[[4,3],4],4],[7,[[8,4],9]]]"

    let i2 = parseLine "[1,1]"

    let exp =
        parseLine "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

    let res = add i1 i2 |> reduce

    Assert.Equal(exp, res)

[<Fact>]
let ``sum1`` () =
    let input =
        "[1,1]
[2,2]
[3,3]
[4,4]"

    Assert.Equal("[[[[1,1],[2,2]],[3,3]],[4,4]]", Pair.print (sum input))

[<Fact>]
let ``sum2`` () =
    let input =
        "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]"

    Assert.Equal("[[[[5,0],[7,4]],[5,5]],[6,6]]", Pair.print (sum input))

[<Fact>]
let ``sum3`` () =
    let input =
        "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"

    Assert.Equal("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", Pair.print (sum input))

[<Theory>]
[<InlineData("[[1,2],[[3,4],5]]", 143)>]
[<InlineData("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384)>]
[<InlineData("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488)>]
let ``magnitude`` (input, expected) =
    Assert.Equal(expected, magnitude (parseLine input))

[<Fact>]
let ``part1`` () =
    let input =
        "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

    Assert.Equal(4140, part1 input)

[<Fact>]
let ``part2`` () =
    let input =
        "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

    Assert.Equal(3993, part2 input)
