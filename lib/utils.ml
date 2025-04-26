open Core

let pp_time fmt time = Format.fprintf fmt "%s" (Time_ns.to_string_utc time)

let pp_time_opt fmt = function
  | None -> Format.fprintf fmt "<empty>"
  | Some time -> Format.fprintf fmt "%s" (Time_ns.to_string_utc time)

let is_date_range_valid start_date end_date =
  let _, _ = (start_date, end_date) in
  let now = Time_ns.now () in
  (*NOTE: to use more complex time utilities (timezones etc.) you have to add
  `core_unix` and `core_unix.time_unix` libraries in your `dune` file.*)
  let today = Time_ns.to_date ~zone:(force Time_float_unix.Zone.local) now in
  printf "%s\n" (Date.to_string today);
  Ok ()

let%expect_test "is_date_range_valid" =
  let _ = is_date_range_valid "" "" in
  [%expect {| 2025-04-26 |}]

(*func CheckDateRange(start, end time.Time) string {*)
(*	today := time.Now()*)
(*	todayDate := time.Date*)
(*		today.Year(),*)
(*		today.Month(),*)
(*		today.Day(),*)
(*		0,*)
(*		0,*)
(*		0,*)
(*		0,*)
(*		today.Location(),*)
(*	)*)
(**)
(*	if start.Before(todayDate) {*)
(*		return fmt.Sprintf("Start date is in the past: %s", start.Format("2006-01-02"))*)
(*	}*)
(**)
(*	if end.Before(start) {*)
(*		return fmt.Sprintf*)
(*			"End date is before start date: Start(%s) - End(%s)",*)
(*			start.Format("2006-01-02"),*)
(*			end.Format("2006-01-02"),*)
(*		)*)
(*	}*)
(**)
(*	return ""*)
(*}*)
