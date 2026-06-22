(* cbor.sig — CBOR (RFC 8949) encoder/decoder signature *)
signature CBOR =
sig
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

  val encode : t -> string

  (* Encode using RFC 8949 Section 4.2.1 Core Deterministic Encoding:
     shortest-form integers/lengths, definite-length items only, and map
     keys sorted in bytewise lexicographic order of their canonical-encoded
     key bytes. Recurses into nested arrays and maps. *)
  val encodeCanonical : t -> string

  val decode : string -> t
end
