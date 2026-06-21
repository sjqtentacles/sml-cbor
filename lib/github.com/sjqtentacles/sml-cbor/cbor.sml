(* cbor.sml — CBOR (RFC 8949) encoder/decoder implementation *)
structure Cbor :> CBOR =
struct

  datatype t
    = Uint   of IntInf.int
    | Nint   of IntInf.int
    | Bytes  of string
    | Text   of string
    | Array  of t list
    | Map    of (t * t) list
    | Tag    of IntInf.int * t
    | Simple of int
    | Float  of real

  (* ------------------------------------------------------------------ *)
  (* Helpers                                                              *)
  (* ------------------------------------------------------------------ *)

  val zero   = IntInf.fromInt 0
  val one    = IntInf.fromInt 1
  val i256   = IntInf.fromInt 256
  val i65536 = IntInf.fromInt 65536
  val i32    = IntInf.fromInt 32

  fun byteStr (n : IntInf.int) : string =
    String.str (Char.chr (IntInf.toInt n))

  (* Encode a CBOR head: major type mt (0-7) and unsigned value n *)
  fun encodeHead (mt : int) (n : IntInf.int) : string =
    let
      val base = IntInf.fromInt (mt * 32)  (* major type in top 3 bits *)
      fun hd ai = byteStr (base + IntInf.fromInt ai)
      fun extract8 v = byteStr (IntInf.mod (v, i256))
      fun div256 v   = IntInf.div (v, i256)
      fun be2 v =
        let val lo = extract8 v
            val hi = extract8 (div256 v)
        in hi ^ lo end
      fun be4 v =
        let val b3 = extract8 v         val r1 = div256 v
            val b2 = extract8 r1        val r2 = div256 r1
            val b1 = extract8 r2        val b0 = extract8 (div256 r2)
        in b0 ^ b1 ^ b2 ^ b3 end
      fun be8 v =
        let val b7 = extract8 v         val r1 = div256 v
            val b6 = extract8 r1        val r2 = div256 r1
            val b5 = extract8 r2        val r3 = div256 r2
            val b4 = extract8 r3        val r4 = div256 r3
            val b3 = extract8 r4        val r5 = div256 r4
            val b2 = extract8 r5        val r6 = div256 r5
            val b1 = extract8 r6        val b0 = extract8 (div256 r6)
        in b0 ^ b1 ^ b2 ^ b3 ^ b4 ^ b5 ^ b6 ^ b7 end
    in
      if n <= IntInf.fromInt 23 then
        byteStr (base + n)
      else if n <= IntInf.fromInt 255 then
        hd 24 ^ extract8 n
      else if n <= IntInf.fromInt 65535 then
        hd 25 ^ be2 n
      else if n <= IntInf.* (IntInf.fromInt 65536, IntInf.fromInt 65536) - one then
        hd 26 ^ be4 n
      else
        hd 27 ^ be8 n
    end

  (* Pack a real (float64) as 8 big-endian bytes using IEEE 754 encoding.
     Computed manually via Real.toManExp and IntInf arithmetic.
     All large constants built by multiplication to stay within 30-bit int range. *)
  fun encodeFloat64 (r : real) : string =
    let
      (* Build large IntInf constants safely from small factors *)
      val pow16   = IntInf.fromInt 65536                (* 2^16 *)
      val pow32   = pow16 * pow16                       (* 2^32 *)
      val pow48   = pow32 * pow16                       (* 2^48 *)
      val pow52   = pow32 * IntInf.fromInt 1048576      (* 2^52 = 2^32 * 2^20 *)
      val pow56   = pow48 * IntInf.fromInt 256          (* 2^56 *)

      val signBit : IntInf.int =
        if Real.signBit r then one else zero

      (* Extract byte k (0 = LSB) from a 64-bit IntInf as a char string *)
      fun byte (v : IntInf.int) (k : int) : string =
        let val shifted = case k of
                            0 => v
                          | 1 => IntInf.div (v, i256)
                          | 2 => IntInf.div (v, pow16)
                          | 3 => IntInf.div (v, IntInf.fromInt 16777216)
                          | 4 => IntInf.div (v, pow32)
                          | 5 => IntInf.div (v, pow32 * i256)
                          | 6 => IntInf.div (v, pow32 * pow16)
                          | 7 => IntInf.div (v, pow56)
                          | _ => zero
        in String.str (Char.chr (IntInf.toInt (IntInf.mod (shifted, i256)))) end

      fun be8 (v : IntInf.int) : string =
        byte v 7 ^ byte v 6 ^ byte v 5 ^ byte v 4 ^
        byte v 3 ^ byte v 2 ^ byte v 1 ^ byte v 0

      fun bits () : IntInf.int =
        if Real.isNan r then
          (* quiet NaN: 0x7FF8000000000000 = 2047*2^52 + 2^51 *)
          IntInf.fromInt 2047 * pow52 + IntInf.div (pow52, IntInf.fromInt 2)
        else if not (Real.isFinite r) then
          (* infinity: (sign * 2^11 + 0x7FF) * 2^52 *)
          (signBit * IntInf.fromInt 2048 + IntInf.fromInt 2047) * pow52
        else if Real.== (Real.abs r, 0.0) then
          signBit * IntInf.fromInt 2048 * pow52
        else
          let
            (* Real.toManExp r = {man, exp} with man in [0.5, 1.0) and r = man * 2^exp *)
            val {man = m, exp = e} = Real.toManExp r
            (* Stored biased exponent = (e - 1) + 1023 *)
            val storedExp = e - 1 + 1023
            (* fraction bits: (2 * |m| - 1) * 2^52, truncated *)
            val fracR = Real.abs m * 2.0 - 1.0
            val fracI : IntInf.int =
              Real.toLargeInt IEEEReal.TO_ZERO
                (fracR * Real.fromLargeInt pow52)
          in
            signBit * IntInf.fromInt 2048 * pow52 +
            IntInf.fromInt storedExp * pow52 +
            fracI
          end
    in
      be8 (bits ())
    end

  (* ------------------------------------------------------------------ *)
  (* Encoder                                                              *)
  (* ------------------------------------------------------------------ *)

  fun encode (item : t) : string =
    case item of
      Uint nv =>
        encodeHead 0 nv
    | Nint nv =>
        encodeHead 1 nv
    | Bytes bv =>
        encodeHead 2 (IntInf.fromInt (String.size bv)) ^ bv
    | Text tv =>
        encodeHead 3 (IntInf.fromInt (String.size tv)) ^ tv
    | Array elems =>
        encodeHead 4 (IntInf.fromInt (List.length elems)) ^
        String.concat (List.map encode elems)
    | Map pairs =>
        encodeHead 5 (IntInf.fromInt (List.length pairs)) ^
        String.concat (List.map (fn (k, v) => encode k ^ encode v) pairs)
    | Tag (tagNum, child) =>
        encodeHead 6 tagNum ^ encode child
    | Simple sv =>
        (* simple value: major type 7 *)
        if sv <= 23 then
          byteStr (IntInf.fromInt (0xe0 + sv))  (* 0xe0 = 7*32 *)
        else if sv <= 255 then
          byteStr (IntInf.fromInt 0xf8) ^ byteStr (IntInf.fromInt sv)
        else
          raise Fail "Simple value out of range"
    | Float rv =>
        (* Always encode as float64 (0xfb) *)
        byteStr (IntInf.fromInt 0xfb) ^ encodeFloat64 rv

  (* ------------------------------------------------------------------ *)
  (* Decoder                                                              *)
  (* ------------------------------------------------------------------ *)

  exception DecodeError of string

  fun decodeFrom (src : string) (pos : int ref) : t =
    let
      val len = String.size src

      fun peekByte () =
        if !pos >= len then raise DecodeError "unexpected end of input"
        else Char.ord (String.sub (src, !pos))

      fun readByte () =
        let val b = peekByte ()
        in pos := !pos + 1; b end

      fun readN (n : int) : string =
        if !pos + n > len then raise DecodeError "truncated input"
        else
          let val s = String.substring (src, !pos, n)
          in pos := !pos + n; s end

      fun bytesToIntInf (bs : string) : IntInf.int =
        let
          fun go (acc, i) =
            if i >= String.size bs then acc
            else go (acc * i256 + IntInf.fromInt (Char.ord (String.sub (bs, i))), i + 1)
        in go (zero, 0) end

      fun readArg (addInfo : int) : IntInf.int =
        if addInfo <= 23 then IntInf.fromInt addInfo
        else if addInfo = 24 then IntInf.fromInt (readByte ())
        else if addInfo = 25 then bytesToIntInf (readN 2)
        else if addInfo = 26 then bytesToIntInf (readN 4)
        else if addInfo = 27 then bytesToIntInf (readN 8)
        else raise DecodeError ("unsupported additional info: " ^ Int.toString addInfo)

      val firstByte = readByte ()
      val mt        = firstByte div 32    (* top 3 bits *)
      val addInfo   = firstByte mod 32    (* bottom 5 bits *)
    in
      case mt of
        0 => Uint (readArg addInfo)
      | 1 => Nint (readArg addInfo)
      | 2 =>
          let val sz  = IntInf.toInt (readArg addInfo)
          in Bytes (readN sz) end
      | 3 =>
          let val sz  = IntInf.toInt (readArg addInfo)
          in Text (readN sz) end
      | 4 =>
          let
            val count = IntInf.toInt (readArg addInfo)
            fun readElems 0 = []
              | readElems remaining =
                  let val elem = decodeFrom src pos
                  in elem :: readElems (remaining - 1) end
          in Array (readElems count) end
      | 5 =>
          let
            val count = IntInf.toInt (readArg addInfo)
            fun readPairs 0 = []
              | readPairs remaining =
                  let val k = decodeFrom src pos
                      val v = decodeFrom src pos
                  in (k, v) :: readPairs (remaining - 1) end
          in Map (readPairs count) end
      | 6 =>
          let
            val tagNum = readArg addInfo
            val child  = decodeFrom src pos
          in Tag (tagNum, child) end
      | 7 =>
          (* Simple values and floats *)
          if addInfo <= 19 then
            Simple addInfo
          else if addInfo = 20 then Simple 20   (* false *)
          else if addInfo = 21 then Simple 21   (* true *)
          else if addInfo = 22 then Simple 22   (* null *)
          else if addInfo = 23 then Simple 23   (* undefined *)
          else if addInfo = 24 then
            Simple (IntInf.toInt (readArg 24))
          else if addInfo = 25 then
            (* float16: read 2 bytes and decode *)
            let
              val b0  = readByte ()
              val b1  = readByte ()
              val sign  = if b0 >= 128 then ~1.0 else 1.0
              val exp16 = (b0 mod 128) div 4
              val mant  = (b0 mod 4) * 256 + b1
            in
              if exp16 = 0 then
                Float (sign * Real.fromInt mant * Math.pow (2.0, ~24.0))
              else if exp16 = 31 then
                if mant = 0 then Float (sign * Real.posInf)
                else Float (0.0 / 0.0)
              else
                Float (sign * (1.0 + Real.fromInt mant / 1024.0) *
                       Math.pow (2.0, Real.fromInt (exp16 - 15)))
            end
          else if addInfo = 26 then
            (* float32: decode IEEE 754 single from 4 bytes big-endian *)
            let
              val bs     = readN 4
              fun ob i   = Char.ord (String.sub (bs, i))
              val b0 = ob 0  val b1 = ob 1  val b2 = ob 2  val b3 = ob 3
              val sign32  = if b0 >= 128 then ~1.0 else 1.0
              val exp32   = (b0 mod 128) * 2 + b1 div 128
              val mant32  = ((b1 mod 128) * 65536) + b2 * 256 + b3
            in
              if exp32 = 0 then
                Float (sign32 * Real.fromInt mant32 * Math.pow (2.0, ~149.0))
              else if exp32 = 255 then
                if mant32 = 0 then Float (sign32 * Real.posInf)
                else Float (0.0 / 0.0)
              else
                Float (sign32 * (1.0 + Real.fromInt mant32 / 8388608.0) *
                       Math.pow (2.0, Real.fromInt (exp32 - 127)))
            end
          else if addInfo = 27 then
            (* float64: decode IEEE 754 double from 8 bytes big-endian *)
            let
              val bs     = readN 8
              fun ob i   = Char.ord (String.sub (bs, i))
              val b0 = ob 0  val b1 = ob 1  val b2 = ob 2  val b3 = ob 3
              val b4 = ob 4  val b5 = ob 5  val b6 = ob 6  val b7 = ob 7
              val sign64 = if b0 >= 128 then ~1.0 else 1.0
              val exp64  = (b0 mod 128) * 16 + b1 div 16
              (* 52-bit mantissa from b1[3:0] b2 b3 b4 b5 b6 b7 *)
              val mHi    = (b1 mod 16) * 65536 + b2 * 256 + b3   (* 20 bits *)
              val mLo32  = b4 * 16777216 + b5 * 65536 + b6 * 256 + b7  (* 32 bits *)
              val mantR  = Real.fromInt mHi * 4294967296.0 + Real.fromInt mLo32
            in
              if exp64 = 0 then
                Float (sign64 * mantR * Math.pow (2.0, ~1074.0))
              else if exp64 = 2047 then
                if mHi = 0 andalso mLo32 = 0 then Float (sign64 * Real.posInf)
                else Float (0.0 / 0.0)
              else
                Float (sign64 * (1.0 + mantR / 4503599627370496.0) *
                       Math.pow (2.0, Real.fromInt (exp64 - 1023)))
            end
          else
            raise DecodeError ("unsupported additional info in major 7: " ^ Int.toString addInfo)
      | _ => raise DecodeError ("unexpected major type: " ^ Int.toString mt)
    end

  fun decode (src : string) : t =
    let val pos = ref 0
    in decodeFrom src pos end

end
