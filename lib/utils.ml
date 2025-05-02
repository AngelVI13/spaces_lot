open Core

let pp_time fmt time = Format.fprintf fmt "%s" (Time_ns.to_string_utc time)

let pp_time_opt fmt = function
  | None -> Format.fprintf fmt "<empty>"
  | Some time -> Format.fprintf fmt "%s" (Time_ns.to_string_utc time)

let pp_date_opt fmt = function
  | None -> Format.fprintf fmt "<empty>"
  | Some time -> Format.fprintf fmt "%s" (Date.to_string time)

let today_date () =
  let now = Time_ns.now () in
  (*NOTE: to use more complex time utilities (timezones etc.) you have to add
  `core_unix` and `core_unix.time_unix` libraries in your `dune` file.*)
  let today = Time_ns.to_date ~zone:(force Time_float_unix.Zone.local) now in
  today

let is_date_range_valid ~today ~start_date ~end_date =
  let start_str = Date.to_string start_date in
  let end_str = Date.to_string end_date in

  (*  NOTE: this could be done nicer with he following but then we don't have detailed
    error messages
    if Date.between ~low:start_date ~high:end_date today *)
  if Date.( < ) start_date today then
    Or_error.error_s [%message "Start date is in the past" (start_str : string)]
  else if Date.( < ) end_date start_date then
    Or_error.error_s
      [%message
        "End date is before start date" (start_str : string) (end_str : string)]
  else Ok ()

(** helper function for testing `is_date_range_valid` *)
let check_date_range ~today ~start ~end_ =
  let start_date = Date.of_string start in
  let end_date = Date.of_string end_ in
  let is_valid = is_date_range_valid ~today ~start_date ~end_date in
  let res =
    match is_valid with
    | Ok _ -> "range is valid"
    | Error err -> Error.to_string_hum err
  in
  printf "%s" res

let%expect_test "is_date_range_valid.valid.1" =
  let today = Date.of_string "2025-04-28" in
  check_date_range ~today ~start:"2025-04-28" ~end_:"2025-04-30";
  [%expect {| range is valid |}]

let%expect_test "is_date_range_valid.start_in_the_past" =
  let today = Date.of_string "2025-04-28" in
  check_date_range ~today ~start:"2025-04-27" ~end_:"2025-04-30";
  [%expect {| ("Start date is in the past" (start_str 2025-04-27)) |}]

let%expect_test "is_date_range_valid.end_before_start" =
  let today = Date.of_string "2025-04-27" in
  check_date_range ~today ~start:"2025-04-28" ~end_:"2025-04-23";
  [%expect
    {| ("End date is before start date" (start_str 2025-04-28) (end_str 2025-04-23)) |}]

let make_floor_str floor =
  let postfix =
    match abs floor with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"
  in
  sprintf "%d%s floor" floor postfix
