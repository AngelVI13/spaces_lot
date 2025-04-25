open Core

module Log = struct
  type level = Debug | Info | Warning | Error
  [@@deriving show { with_path = false }]

  let _format_output level s =
    let now = Time_ns.now () |> Time_ns.to_string_utc in
    sprintf "%s: %s: %s" now (show_level level) s

  let _log level fmt =
    let out = Printf.ksprintf (_format_output level) fmt in
    printf "%s" out

  let debug = _log Debug
  let info = _log Info
  let warn = _log Warning
  let error = _log Error
end
