open! Core

module Lot = struct
  let do_sth = ()

  let%expect_test "addition_game" =
    printf "%d" (2 + 2);
    [%expect {| 4 |}]
end
