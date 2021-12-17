module Day17Tests

open Xunit

[<Fact>]
let ``finalY`` () = Assert.Equal(-9, Day17.finalY 9 3)

[<Theory>]
[<InlineData(7, 7, 28)>]
[<InlineData(3, 7, 18)>]
[<InlineData(9, 6, 21)>]
let ``finalX`` (steps, initial, expected) =
    Assert.Equal(expected, Day17.finalX steps initial)
