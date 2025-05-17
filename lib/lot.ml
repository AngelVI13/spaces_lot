open Core
open Space

module Lot = struct
  (*TODO: define type type ReleaseMap map[SpaceKey]*ReleasePool *)
  type t = { filename : string; unit_spaces : Space.t Map.M(Space_key).t }
  [@@deriving sexp]

  let pp fmt (t : t) =
    let open Format in
    let pp_kv fmt (k, v) =
      fprintf fmt "@[<2>%s -> %a@]" (Space_key.show k) Space.pp v
    in
    fprintf fmt "@[<v>{ filename = %s;@ unit_spaces = {@;<1 2>%a }@] }"
      t.filename (pp_print_list pp_kv)
      (Map.to_alist t.unit_spaces)

  let%expect_test "test_map_creation_and_pp" =
    let space = Space.make ~floor:1 ~number:120 ~description:"test space" in
    let lot =
      {
        filename = "filename.json";
        unit_spaces = Map.of_alist_exn (module Space_key) [ (space.key, space) ];
      }
    in
    Format.printf "%a" pp lot;
    [%expect
      {|
      { filename = filename.json;
      unit_spaces = {
        "1st floor 120" -> { Space.Space.number = 120; floor = 1;
                             key = "1st floor 120"; description = "test space";
                             reserved = false; autoRelease = false;
                             reservedBy = ""; reservedById = "";
                             reservedTime = 2025-05-17 20:36:01.546454292+03:00 } } }
      |}]
end
