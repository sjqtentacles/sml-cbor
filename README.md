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

(* Deterministic (canonical) encoding: sorts map keys, definite lengths *)
val canon = encodeCanonical v
```

## API (`signature CBOR`)

```sml
datatype t
  = Uint   of IntInf.int    (* unsigned integer        *)
  | Nint   of IntInf.int    (* negative integer -1 - n  *)
  | Bytes  of string        (* byte string             *)
  | Text   of string        (* UTF-8 text string       *)
  | Array  of t list
  | Map    of (t * t) list
  | Tag    of IntInf.int * t
  | Simple of int
  | Float  of real

val encode          : t -> string   (* shortest-form, definite-length *)
val encodeCanonical : t -> string   (* RFC 8949 Section 4.2.1 deterministic *)
val decode          : string -> t
```

### RFC 8949 deterministic encoding

`encodeCanonical` produces the Core Deterministic Encoding of RFC 8949
Section 4.2.1: integers and lengths use the shortest (preferred) form, every
item is definite-length, and **map keys are sorted in bytewise lexicographic
order of their canonical-encoded key bytes** (each key is encoded, then entries
are ordered by comparing those byte strings byte by byte, with a shorter prefix
sorting first). It recurses into nested arrays and maps, so the whole structure
is canonicalized. On values without maps it is byte-for-byte identical to
`encode`. For example, a map with keys `10, 100, -1, "z", "aa"` always serializes
its keys in that order regardless of insertion order.

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
