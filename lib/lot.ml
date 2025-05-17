open! Core
open Space

(*TODO: how to create a map from an interface type i.e. type behind a .mli file?
It looks like you just have to expose the comparator witness type -> TEST THIS
 *)
module SpaceKey1 = struct
  module T = struct
    type t = string [@@deriving show, sexp, compare]

    let make ~floor ~number =
      sprintf "%s %d" (Utils.make_floor_str floor) number
  end

  include T
  include Comparator.Make (T)
end

module Lot = struct
  (*TODO: define type type ReleaseMap map[SpaceKey]*ReleasePool *)
  type t = { filename : string; unit_spaces : Space.t Map.M(Space_key).t }
  [@@deriving show, sexp]
  (*type t = {*)
  (*  filename : string;*)
  (*  unit_spaces : (Space_key.t, Space.t, Space_key.comparator_witness) Map.t;*)
  (*}*)

  let%expect_test "addition_game" =
    printf "%d" (2 + 2);
    [%expect {| 4 |}]
end
