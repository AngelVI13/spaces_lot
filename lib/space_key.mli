open Core

type t
type comparator_witness

val make: floor:int -> number:int -> t
val pp: Format.formatter -> t -> unit
val show: t -> string
val sexp_of_t: t -> Sexp.t
val t_of_sexp: Sexp.t -> t


