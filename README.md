# sml-cbor

CBOR (Concise Binary Object Representation) encoder/decoder in pure Standard ML (RFC 8949)

## Installation

```
smlpkg add github.com/sjqtentacles/sml-cbor
smlpkg sync
```

## Usage

```sml
open Cbor

(* Build a CBOR value *)
val v = Map [ (Text "name",  Text "Alice")
            , (Text "age",   Uint (IntInf.fromInt 30))
            , (Text "scores", Array [Uint (IntInf.fromInt 95), Uint (IntInf.fromInt 87)])
            ]

(* Encode to bytes *)
val bytes = encode v

(* Decode from bytes *)
val decoded = decode bytes
(* => Map [("name", "Alice"), ("age", 30), ...] *)

(* Primitive types *)
val _ = encode (Uint (IntInf.fromInt 42))
val _ = encode (Nint (IntInf.fromInt 1))   (* negative: -1 - n *)
val _ = encode (Bytes "\xde\xad\xbe\xef")
val _ = encode (Float 3.14)
val _ = encode (Simple 20)                 (* false = Simple 20, true = Simple 21 *)
```

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
