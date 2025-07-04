open Core
open Import

module Space = struct
  type t = {
    number : int;
    floor : int;
    key : Space_key.t;
    description : string;
    reserved : bool;
    autoRelease : bool;
    reservedBy : string;
    reservedById : string;
    reservedTime : (Time_ns.t[@opaque]);
  }
  [@@deriving show, sexp]

  let make ~number ~floor ~description =
    {
      number;
      floor;
      key = Space_key.make ~floor ~number;
      description;
      reserved = false;
      autoRelease = false;
      reservedBy = "";
      reservedById = "";
      reservedTime = Time_ns.now ();
    }

  let props_txt s =
    let desc = match s.description with "" -> "" | x -> sprintf " - %s" x in
    sprintf "(%d floor%s)" s.floor desc

  let status_emoji s =
    if s.reserved then ":large_orange_circle:" else ":large_green_circle:"

  (** returns empty string if space is free, otherwise returns who reserved it*)
  let status_description s =
    if s.reserved then sprintf "<@%s>" s.reservedById else ""

  let is_smaller s other =
    match s with
    | _ when s.floor < other.floor -> true
    | _ when s.floor > other.floor -> false
    | _ when s.floor = other.floor -> s.number < other.number
    | _ ->
        failwith (sprintf "failed to compare %s with %s" (show s) (show other))

  (*TODO: this is not used and can be remove*)
  let pp_custom fmt (s : t) =
    Format.fprintf fmt "@[<v>{ number = %d;@ floor = %d;@ description = %s;@] }"
      s.number s.floor s.description
end

let%expect_test "props_txt" =
  let s = Space.make ~number:1 ~floor:4 ~description:"Verification Room" in
  printf "%s" (Space.props_txt s);
  [%expect {| (4 floor - Verification Room) |}]

let%expect_test "is_smaller" =
  let s1 = Space.make ~number:1 ~floor:4 ~description:"Verification Room" in
  let s2 = Space.make ~number:2 ~floor:4 ~description:"Verification Room" in
  printf "%B" (Space.is_smaller s1 s2);
  [%expect {| true |}];

  let s1 = Space.make ~number:2 ~floor:4 ~description:"Verification Room" in
  let s2 = Space.make ~number:1 ~floor:4 ~description:"Verification Room" in
  printf "%B" (Space.is_smaller s1 s2);
  [%expect {| false |}];

  let s1 = Space.make ~number:1 ~floor:3 ~description:"Verification Room" in
  let s2 = Space.make ~number:1 ~floor:4 ~description:"Verification Room" in
  printf "%B" (Space.is_smaller s1 s2);
  [%expect {| true |}];

  let s1 = Space.make ~number:1 ~floor:4 ~description:"Verification Room" in
  let s2 = Space.make ~number:1 ~floor:3 ~description:"Verification Room" in
  printf "%B" (Space.is_smaller s1 s2);
  [%expect {| false |}];

  let s1 = Space.make ~number:2 ~floor:3 ~description:"Verification Room" in
  let s2 = Space.make ~number:1 ~floor:4 ~description:"Verification Room" in
  printf "%B" (Space.is_smaller s1 s2);
  [%expect {| true |}]
