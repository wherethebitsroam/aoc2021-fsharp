module Day22Tests

open Xunit
open Day22

[<Theory>]
// no overlap
[<InlineData(1, 2, 4, 5, false, 0, 0)>]
// half overlap
[<InlineData(1, 4, 2, 5, true, 2, 4)>]
// half overlap other way
[<InlineData(2, 5, 1, 4, true, 2, 4)>]
// full overlap
[<InlineData(1, 5, 2, 4, true, 2, 4)>]
// full overlap other way
[<InlineData(2, 4, 1, 5, true, 2, 4)>]
let ``Test Range.overlap`` (a1, a2, b1, b2, eIsSome, e1, e2) =
    if eIsSome then
        Assert.Equal(Some(e1, e2), Range.overlap (a1, a2) (b1, b2))
    else
        Assert.Equal(None, Range.overlap (a1, a2) (b1, b2))

[<Fact>]
let ``Test Cube.overlap`` () =
    let c1 =
        { x = (0, 10)
          y = (0, 10)
          z = (0, 10) }

    let c2 =
        { x = (20, 30)
          y = (20, 30)
          z = (20, 30) }

    Assert.Equal(None, Cube.overlap c1 c2)

[<Fact>]
let ``Test Cube.overlap2`` () =
    let c1 =
        { x = (0, 10)
          y = (0, 10)
          z = (0, 10) }

    let c2 =
        { x = (5, 15)
          y = (5, 15)
          z = (5, 15) }

    let expected =
        { x = (5, 10)
          y = (5, 10)
          z = (5, 10) }

    Assert.Equal(Some expected, Cube.overlap c1 c2)
