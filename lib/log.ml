open Core

(*TODO: add support for writing to file if thats how the user setup the logging*)
(*TODO: add support for different time/message formats *)
type level = Debug | Info | Warning | Error
[@@deriving show { with_path = false }]

(* NOTE: here fmt is a polymorphic format type. It has to come as last argument
   because ocaml treats it in a special way *)
let _log level fmt =
  (* NOTE: Here we can also use Printf.ksprintf and the result will be
     identical. Format.kasprintf is more powerful and allows for advanced
     pretty printing, indentations etc.*)
  Format.kasprintf
    (fun s ->
      let now = Time_ns.now () |> Time_ns.to_string_utc in
      printf "%s: %s: %s\n" now (show_level level) s)
    fmt

(* NOTE: Here we have to specify the fmt argument and explicitly pass it down to our
   closure because it's a special polymorphic format type *)
let debug fmt = _log Debug fmt
let info fmt = _log Info fmt
let warn fmt = _log Warning fmt
let error fmt = _log Error fmt
