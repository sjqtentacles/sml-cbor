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
  val decode : string -> t
end
