module BitReader

// type BitReader = { bytes: byte []; byte: int; bit: int }

// type BitAction<'a> = BitAction of (BitReader -> 'a)

// let create (bytes: byte []) =
//     { bytes = bytes; byte = 0; bit = 8 }, ()

// let mask from len =
//     ((1 <<< len) - 1) <<< (from - len) |> byte

// let readBits (b: byte) from len = (b &&& mask from len) >>> (from - len)

// let readPartial (n: int) =
//     let action (b: BitReader) =
//         if n > b.bit then
//             failwithf "trying to read %d when bit is %d" n b.bit

//         readBits b.bytes.[b.byte] b.bit n |> int

//     BitAction action


// let read (n: int) (r: BitReader<'a>) =
//     let mutable rdr = r
//     let mutable n = n
//     let mutable res = 0

//     while n > 0 do
//         let rn = [ rdr.bit; n ] |> List.min
//         let (x, r) = readPartial rn rdr
//         res <- res ||| (x <<< (n - rn))
//         n <- n - rn
//         rdr <- r

//     (res, rdr)
