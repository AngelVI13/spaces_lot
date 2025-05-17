open Core

module T = struct
  (*NOTE: to automatically generate interface definition from this file you can run*)
  (*the following command*)
  (*`ocaml-print-intf PATH_TO_ML_FILE`*)
  (*Make sure to install this package first (via opam)*)
  type t = string [@@deriving show, sexp, compare]

  (*NOTE: since we are deriving show and sexp there are a few functions*)
  (*automatically generated for our type. If we want those to be exposed then we*)
  (*just have to add them to the mli file but you don't have to redefine them here*)
  let make ~floor ~number = sprintf "%s %d" (Utils.make_floor_str floor) number
end

include T
include Comparator.Make (T)
