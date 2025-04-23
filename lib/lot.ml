open! Core

(*NOTE: inside ReleaseInfo do not take pointer to space but instead reference a*)
(*space by it's SpaceKey*)

module Lot = struct
  (*TODO: define type UnitSpaces map[SpaceKey]*Space *)
  (*TODO: define type type ReleaseMap map[SpaceKey]*ReleasePool *)
  type t = { filename : string }

  let%expect_test "addition_game" =
    printf "%d" (2 + 2);
    [%expect {| 4 |}]
end
