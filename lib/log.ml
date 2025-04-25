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

let shout fmt = Printf.ksprintf (fun s -> s ^ "!") fmt
let logv fmt = printf "%s" (Printf.ksprintf (fun s -> s ^ "!") fmt)
let my_printf = Format.kfprintf (fun _ -> ()) Format.std_formatter

let%expect_test "log.debug" =
  Log.debug "hello world";
  [%expect {| 2025-04-25 13:15:13.773326572Z: Debug: hello world |}]

let%expect_test "log.debug with format" =
  let x = "world" in
  let res = shout "hello %s" x in
  printf "%s" res;
  [%expect {| hello world! |}]

let%expect_test "gptk with format" =
  my_printf "Number: %d, Word: %s\n" 42 "hello";
  [%expect {| Number: 42, Word: hello |}]
