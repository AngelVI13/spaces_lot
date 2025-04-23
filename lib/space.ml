open Core

type skey = SpaceKey of string

let make_floor_str floor =
  let postfix =
    match abs floor with 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"
  in
  sprintf "%d%s floor" floor postfix

(*TODO: should this be a separate struct or just part of `t`*)
type reservedProps = {
  reserved : bool;
  autoRelease : bool;
  reservedBy : string;
  reservedById : string;
  reservedTime : Core.Time_ns.t;
}
(*[@@deriving show]*)
(*how to derive show for Time_ns.t*)

module Space = struct
  type t = {
    number : int;
    floor : int;
    description : string;
    resProps : reservedProps;
  }

  let make ~number ~floor ~description =
    {
      number;
      floor;
      description;
      resProps =
        {
          reserved = false;
          autoRelease = false;
          reservedBy = "";
          reservedById = "";
          reservedTime = Time_ns.now ();
        };
    }

  let props_txt s =
    let desc = match s.description with "" -> "" | x -> sprintf " - %s" x in
    sprintf "(%d floor%s)" s.floor desc

  let status_emoji s =
    if s.reserved then ":large_orange_circle:" else ":large_green_circle:"

  (** returns empty string if space is free, otherwise returns who reserved it*)
  let status_description s =
    if s.reserved then sprintf "<@%s>" s.reservedById else ""

  let key s = SpaceKey (sprintf "%s %d" (make_floor_str s.floor) s.number)

  let is_smaller s other =
    match s with
    | _ when s.floor < other.floor -> true
    | _ when s.floor > other.floor -> false
    | _ when s.floor = other.floor -> s.number < other.number
    (*| _ -> failwith (sprintf "failed to compare %s with %s" s other)*)
    (*| _ -> failwith (sprintf "failed to compare %s with %s" (key s) (key other))*)
    | _ -> failwith (sprintf "failed to compare spaces")
  (*TODO: fix this to print proper space info*)
end

(*TODO: Remaining convertions*)
(**)
(*func (p *Space) Smaller(other *Space) bool {*)
(*	if p.Floor < other.Floor {*)
(*		return true*)
(*	} else if p.Floor > other.Floor {*)
(*		return false*)
(*	} else { // p.Floor == other.Floor*)
(*		return p.Number < other.Number*)
(*	}*)
(*}*)
(**)
(**)

let%expect_test "props_txt" =
  let s = Space.make ~number:1 ~floor:4 ~description:"Verification Room" in
  printf "%s" (Space.props_txt s);
  [%expect {| (4 floor - Verification Room) |}]
