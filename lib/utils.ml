open Core

let pp_time fmt time = Format.fprintf fmt "%s" (Time_ns.to_string_utc time)

let pp_time_opt fmt = function
  | None -> Format.fprintf fmt "<empty>"
  | Some time -> Format.fprintf fmt "%s" (Time_ns.to_string_utc time)
