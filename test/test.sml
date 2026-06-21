(* test.sml — CBOR test suite covering RFC 8949 Appendix A vectors *)
structure Tests =
struct

  (* Helper: convert a string to a hex dump for error messages *)
  fun hexDump (s : string) : string =
    String.concat
      (List.map
        (fn c =>
          let val n = Char.ord c
              val hi = n div 16
              val lo = n mod 16
              fun hex d = if d < 10 then Char.chr (d + 48)
                          else Char.chr (d + 87)
          in String.implode [hex hi, hex lo, #" "] end)
        (String.explode s))

  fun checkEncode (name : string) (item : Cbor.t) (expected : string) =
    let val actual = Cbor.encode item
    in
      if actual = expected then
        Harness.check name true
      else
        ( print ("  FAIL - " ^ name ^ ": expected " ^ hexDump expected ^
                 " got " ^ hexDump actual ^ "\n")
        ; Harness.check name false )
    end

  fun checkDecode (name : string) (encoded : string) (expected : Cbor.t) =
    let val actual = Cbor.decode encoded
    in
      if Cbor.encode actual = Cbor.encode expected then
        Harness.check name true
      else
        Harness.check name false
    end

  (* Build a one-byte string from an int 0-255 *)
  fun b (n : int) : string = String.str (Char.chr n)
  fun bs (ns : int list) : string = String.concat (List.map b ns)

  fun run () =
    let
      val () = Harness.reset ()

      (* ---------------------------------------------------------------- *)
      (* Section 1: Unsigned integers (major type 0)                      *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Uint encoding"

      (* 1. Uint 0 -> 0x00 *)
      val () = checkEncode "Uint 0" (Cbor.Uint (IntInf.fromInt 0)) (b 0x00)

      (* 2. Uint 1 -> 0x01 *)
      val () = checkEncode "Uint 1" (Cbor.Uint (IntInf.fromInt 1)) (b 0x01)

      (* 3. Uint 10 -> 0x0a *)
      val () = checkEncode "Uint 10" (Cbor.Uint (IntInf.fromInt 10)) (b 0x0a)

      (* 4. Uint 23 -> 0x17 *)
      val () = checkEncode "Uint 23" (Cbor.Uint (IntInf.fromInt 23)) (b 0x17)

      (* 5. Uint 24 -> 0x18 0x18 *)
      val () = checkEncode "Uint 24" (Cbor.Uint (IntInf.fromInt 24)) (bs [0x18, 0x18])

      (* 6. Uint 25 -> 0x18 0x19 *)
      val () = checkEncode "Uint 25" (Cbor.Uint (IntInf.fromInt 25)) (bs [0x18, 0x19])

      (* 7. Uint 100 -> 0x18 0x64 *)
      val () = checkEncode "Uint 100" (Cbor.Uint (IntInf.fromInt 100)) (bs [0x18, 0x64])

      (* 8. Uint 1000 -> 0x19 0x03 0xe8 *)
      val () = checkEncode "Uint 1000" (Cbor.Uint (IntInf.fromInt 1000)) (bs [0x19, 0x03, 0xe8])

      (* ---------------------------------------------------------------- *)
      (* Section 2: Negative integers (major type 1)                      *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Nint encoding"

      (* 9. Nint 0 (i.e. -1) -> 0x20 *)
      val () = checkEncode "Nint(-1) encoded as Nint 0" (Cbor.Nint (IntInf.fromInt 0)) (b 0x20)

      (* 10. Nint 9 (i.e. -10) -> 0x29 *)
      val () = checkEncode "Nint(-10) encoded as Nint 9" (Cbor.Nint (IntInf.fromInt 9)) (b 0x29)

      (* ---------------------------------------------------------------- *)
      (* Section 3: Byte strings (major type 2)                           *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Bytes encoding"

      (* 11. Bytes "" -> 0x40 *)
      val () = checkEncode "Bytes empty" (Cbor.Bytes "") (b 0x40)

      (* 12. Bytes "\x01\x02\x03\x04" -> 0x44 0x01 0x02 0x03 0x04 *)
      val () = checkEncode "Bytes 4 bytes"
                 (Cbor.Bytes (bs [0x01, 0x02, 0x03, 0x04]))
                 (bs [0x44, 0x01, 0x02, 0x03, 0x04])

      (* ---------------------------------------------------------------- *)
      (* Section 4: Text strings (major type 3)                           *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Text encoding"

      (* 13. Text "" -> 0x60 *)
      val () = checkEncode "Text empty" (Cbor.Text "") (b 0x60)

      (* 14. Text "a" -> 0x61 0x61 *)
      val () = checkEncode "Text \"a\"" (Cbor.Text "a") (bs [0x61, 0x61])

      (* 15. Text "IETF" -> 0x64 0x49 0x45 0x54 0x46 *)
      val () = checkEncode "Text \"IETF\""
                 (Cbor.Text "IETF")
                 (bs [0x64, 0x49, 0x45, 0x54, 0x46])

      (* ---------------------------------------------------------------- *)
      (* Section 5: Arrays (major type 4)                                 *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Array encoding"

      (* 16. Array [] -> 0x80 *)
      val () = checkEncode "Array empty" (Cbor.Array []) (b 0x80)

      (* 17. Array [1,2,3] -> 0x83 0x01 0x02 0x03 *)
      val () = checkEncode "Array [1,2,3]"
                 (Cbor.Array [ Cbor.Uint (IntInf.fromInt 1)
                              , Cbor.Uint (IntInf.fromInt 2)
                              , Cbor.Uint (IntInf.fromInt 3) ])
                 (bs [0x83, 0x01, 0x02, 0x03])

      (* ---------------------------------------------------------------- *)
      (* Section 6: Maps (major type 5)                                   *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Map encoding"

      (* 18. Map [] -> 0xa0 *)
      val () = checkEncode "Map empty" (Cbor.Map []) (b 0xa0)

      (* ---------------------------------------------------------------- *)
      (* Section 7: Simple values (major type 7)                          *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Simple values"

      (* 19. false -> 0xf4 *)
      val () = checkEncode "Simple false" (Cbor.Simple 20) (b 0xf4)

      (* 20. true -> 0xf5 *)
      val () = checkEncode "Simple true" (Cbor.Simple 21) (b 0xf5)

      (* 21. null -> 0xf6 *)
      val () = checkEncode "Simple null" (Cbor.Simple 22) (b 0xf6)

      (* ---------------------------------------------------------------- *)
      (* Section 8: Decode tests                                          *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Decode"

      (* 22. decode(encode(x)) roundtrip for Uint 0 *)
      val () = checkDecode "Decode Uint 0"  (b 0x00) (Cbor.Uint (IntInf.fromInt 0))
      val () = checkDecode "Decode Uint 1"  (b 0x01) (Cbor.Uint (IntInf.fromInt 1))
      val () = checkDecode "Decode Uint 10" (b 0x0a) (Cbor.Uint (IntInf.fromInt 10))
      val () = checkDecode "Decode Uint 23" (b 0x17) (Cbor.Uint (IntInf.fromInt 23))
      val () = checkDecode "Decode Uint 24" (bs [0x18, 0x18]) (Cbor.Uint (IntInf.fromInt 24))
      val () = checkDecode "Decode Uint 1000" (bs [0x19, 0x03, 0xe8]) (Cbor.Uint (IntInf.fromInt 1000))
      val () = checkDecode "Decode Nint(-1)" (b 0x20) (Cbor.Nint (IntInf.fromInt 0))
      val () = checkDecode "Decode Bytes empty" (b 0x40) (Cbor.Bytes "")
      val () = checkDecode "Decode Text empty" (b 0x60) (Cbor.Text "")
      val () = checkDecode "Decode Text IETF"
                 (bs [0x64, 0x49, 0x45, 0x54, 0x46])
                 (Cbor.Text "IETF")
      val () = checkDecode "Decode Array empty" (b 0x80) (Cbor.Array [])
      val () = checkDecode "Decode Map empty" (b 0xa0) (Cbor.Map [])
      val () = checkDecode "Decode Simple false" (b 0xf4) (Cbor.Simple 20)
      val () = checkDecode "Decode Simple true"  (b 0xf5) (Cbor.Simple 21)
      val () = checkDecode "Decode Simple null"  (b 0xf6) (Cbor.Simple 22)

      (* ---------------------------------------------------------------- *)
      (* Section 9: Roundtrip encode/decode                               *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Roundtrip"

      fun roundtrip name item =
        let val enc = Cbor.encode item
            val dec = Cbor.decode enc
        in
          Harness.check name (Cbor.encode dec = enc)
        end

      val () = roundtrip "RT Uint 0"    (Cbor.Uint (IntInf.fromInt 0))
      val () = roundtrip "RT Uint 100"  (Cbor.Uint (IntInf.fromInt 100))
      val () = roundtrip "RT Uint 1000" (Cbor.Uint (IntInf.fromInt 1000))
      val () = roundtrip "RT Nint 0"    (Cbor.Nint (IntInf.fromInt 0))
      val () = roundtrip "RT Nint 9"    (Cbor.Nint (IntInf.fromInt 9))
      val () = roundtrip "RT Bytes"     (Cbor.Bytes (bs [0x01, 0x02, 0x03, 0x04]))
      val () = roundtrip "RT Text IETF" (Cbor.Text "IETF")
      val () = roundtrip "RT Array 123"
                 (Cbor.Array [ Cbor.Uint (IntInf.fromInt 1)
                              , Cbor.Uint (IntInf.fromInt 2)
                              , Cbor.Uint (IntInf.fromInt 3) ])
      val () = roundtrip "RT Map"
                 (Cbor.Map [ (Cbor.Text "a", Cbor.Uint (IntInf.fromInt 1))
                            , (Cbor.Text "b", Cbor.Uint (IntInf.fromInt 2)) ])
      val () = roundtrip "RT Simple false" (Cbor.Simple 20)
      val () = roundtrip "RT Simple true"  (Cbor.Simple 21)
      val () = roundtrip "RT Simple null"  (Cbor.Simple 22)
      val () = roundtrip "RT Tag"
                 (Cbor.Tag (IntInf.fromInt 1, Cbor.Uint (IntInf.fromInt 1363896240)))

      (* ---------------------------------------------------------------- *)
      (* Section 10: Large integer encoding                               *)
      (* ---------------------------------------------------------------- *)
      val () = Harness.section "Large integers"

      val () = checkEncode "Uint 65535" (Cbor.Uint (IntInf.fromInt 65535))
                 (bs [0x19, 0xff, 0xff])
      val () = checkEncode "Uint 65536" (Cbor.Uint (IntInf.fromInt 65536))
                 (bs [0x1a, 0x00, 0x01, 0x00, 0x00])
      val () = checkEncode "Uint 4294967295"
                 (Cbor.Uint (IntInf.* (IntInf.fromInt 65536, IntInf.fromInt 65536) - IntInf.fromInt 1))
                 (bs [0x1a, 0xff, 0xff, 0xff, 0xff])

    in
      Harness.run ()
    end

end
