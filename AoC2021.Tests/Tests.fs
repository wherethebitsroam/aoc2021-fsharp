module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``Test mask`` () =
    Assert.Equal(byte 0b111, Bytecode.mask 3 3)
    Assert.Equal(byte 0b11100, Bytecode.mask 5 3)

[<Fact>]
let ``Test readBits`` () =
    let b = byte 0b10010110
    Assert.Equal(byte 0b100, Bytecode.readBits b 8 3)
    Assert.Equal(byte 0b0101, Bytecode.readBits b 6 4)

[<Fact>]
let ``Test readPartial`` () =
    let r =
        Bytecode.reader [| byte 0b10010110
                           byte 0b10010110 |]

    let (res, r) = Bytecode.readPartial 3 r
    Assert.Equal(0b100, res)
    Assert.Equal(0, r.byte)
    Assert.Equal(5, r.bit)

    let (res, r) = Bytecode.readPartial 3 r
    Assert.Equal(0b101, res)
    Assert.Equal(0, r.byte)
    Assert.Equal(2, r.bit)

    let (res, r) = Bytecode.readPartial 2 r
    Assert.Equal(0b10, res)
    Assert.Equal(1, r.byte)
    Assert.Equal(8, r.bit)

[<Fact>]
let ``Test read`` () =
    let r =
        Bytecode.reader [| byte 0b10010110
                           byte 0b10010110
                           byte 0b10010110 |]

    let (res, r) = Bytecode.read 7 r
    Assert.Equal(0b1001011, res)
    Assert.Equal(0, r.byte)
    Assert.Equal(1, r.bit)

    let (res, r) = Bytecode.read 10 r
    Assert.Equal(0b0100101101, res) // 301
    Assert.Equal(2, r.byte)
    Assert.Equal(7, r.bit)

let assertLiteral n (packet: Bytecode.Packet) =
    match packet.value with
    | Bytecode.Literal x -> Assert.Equal(n, x)
    | _ -> failwith "not literal"

[<Fact>]
let ``Test readPacket`` () =
    let r =
        Bytecode.reader [| byte 0b11101110
                           byte 0b00000000
                           byte 0b11010100
                           byte 0b00001100
                           byte 0b10000010
                           byte 0b00110000
                           byte 0b01100000 |]

    let (p, _) = Bytecode.readPacket r

    Assert.Equal(7, p.version)

    match p.value with
    | Bytecode.Operator (_, packets) ->
        Assert.Equal(3, List.length packets)

        List.zip packets [ 1; 2; 3 ]
        |> List.iter (fun (p, expected) -> assertLiteral expected p)

    | _ -> failwith "bad value"

[<Fact>]
let ``Test readPacket length op`` () =
    let r =
        Bytecode.reader [| byte 0b00111000
                           byte 0b00000000
                           byte 0b01101111
                           byte 0b01000101
                           byte 0b00101001
                           byte 0b00010010
                           byte 0b00000000 |]

    let (p, _) = Bytecode.readPacket r

    Assert.Equal(1, p.version)

    match p.value with
    | Bytecode.Operator (_, packets) ->
        Assert.Equal(2, List.length packets)

        List.zip packets [ 10; 20 ]
        |> List.iter (fun (p, expected) -> assertLiteral expected p)

    | _ -> failwith "bad value"

[<Theory>]
[<InlineData("8A004A801A8002F478", 16)>]
[<InlineData("620080001611562C8802118E34", 12)>]
[<InlineData("C0015000016115A2E0802F182340", 23)>]
[<InlineData("A0016C880162017C3686B18A3D4780", 31)>]
let ``Test Day16 part1`` (s, expected) = Assert.Equal(expected, Day16.part1 s)

[<Theory>]
[<InlineData("C200B40A82", 3)>]
[<InlineData("04005AC33890", 54)>]
[<InlineData("880086C3E88112", 7)>]
[<InlineData("CE00C43D881120", 9)>]
[<InlineData("D8005AC2A8F0", 1)>]
[<InlineData("F600BC2D8F", 0)>]
[<InlineData("9C0141080250320F1802104A08", 1)>]
let ``Test Day16 part2`` (s, expected) = Assert.Equal(expected, Day16.part2 s)

let packet value : Bytecode.Packet = { version = 0; len = 0; value = value }

let op op ps = packet (Bytecode.Operator(op, ps))

let lit x = packet (Bytecode.Literal x)

[<Fact>]
let ``Test eval`` () =
    let p = op Bytecode.Sum [ lit 4; lit 8; lit 9 ]
    Assert.Equal(21L, Day16.eval p)

    let p =
        op Bytecode.Product [ lit 2; lit 3; lit 4 ]

    Assert.Equal(24L, Day16.eval p)

    let p = op Bytecode.Min [ lit 2; lit 3; lit 4 ]
    Assert.Equal(2L, Day16.eval p)

    let p = op Bytecode.Max [ lit 2; lit 3; lit 4 ]
    Assert.Equal(4L, Day16.eval p)

    let p = op Bytecode.Gt [ lit 2; lit 2 ]
    Assert.Equal(0L, Day16.eval p)

    let p = op Bytecode.Gt [ lit 2; lit 3 ]
    Assert.Equal(0L, Day16.eval p)

    let p = op Bytecode.Gt [ lit 3; lit 2 ]
    Assert.Equal(1L, Day16.eval p)

    let p = op Bytecode.Lt [ lit 2; lit 2 ]
    Assert.Equal(0L, Day16.eval p)

    let p = op Bytecode.Lt [ lit 2; lit 3 ]
    Assert.Equal(1L, Day16.eval p)

    let p = op Bytecode.Lt [ lit 3; lit 2 ]
    Assert.Equal(0L, Day16.eval p)

    let p = op Bytecode.Eq [ lit 2; lit 2 ]
    Assert.Equal(1L, Day16.eval p)

    let p = op Bytecode.Eq [ lit 3; lit 2 ]
    Assert.Equal(0L, Day16.eval p)

[<Fact>]
let ``Test Lit`` () =
    Assert.Equal(2021L, Day16.part2 "D2FE28")
