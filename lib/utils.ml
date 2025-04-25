open Core

let pp_time fmt time = Format.fprintf fmt "%s" (Time_ns.to_string_utc time)

let pp_time_opt fmt = function
  | None -> Format.fprintf fmt "<empty>"
  | Some time -> Format.fprintf fmt "%s" (Time_ns.to_string_utc time)

let is_date_range_valid start_date end_date =
  let _, _ = (start_date, end_date) in
  Ok ()

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
