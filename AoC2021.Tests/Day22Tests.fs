module Day22Tests

open Xunit
open Day22

[<Fact>]
let ``Test overlap`` () =
    let c1 =
        { onOff = On
          x = (0, 10)
          y = (0, 10)
          z = (0, 10) }

    let c2 =
        { onOff = On
          x = (20, 30)
          y = (20, 30)
          z = (20, 30) }

    Assert.Equal(None, Cube.overlap c1 c2)

[<Fact>]
let ``Test overlap2`` () =
    let c1 =
        { onOff = On
          x = (0, 10)
          y = (0, 10)
          z = (0, 10) }

    let c2 =
        { onOff = Off
          x = (5, 15)
          y = (5, 15)
          z = (5, 15) }

    let expected =
        { onOff = Off
          x = (5, 10)
          y = (5, 10)
          z = (5, 10) }

    Assert.Equal(Some expected, Cube.overlap c1 c2)
