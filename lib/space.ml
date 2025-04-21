open Core

type reservedProps = {
  reserved : bool;
  autoRelease : bool;
  reservedBy : string;
  reservedById : string;
  reservedTime : Core.Time_ns.t;
}

module Space = struct
  type t = {
    number : int;
    floor : int;
    description : string;
    resProps : reservedProps;
  }

  let make number floor description =
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
end

(*TODO: Remaining convertions*)
(*func (p *Space) GetStatusEmoji() string {*)
(*	emoji := ":large_green_circle:"*)
(*	if p.Reserved {*)
(*		emoji = ":large_orange_circle:"*)
(*	}*)
(*	return emoji*)
(*}*)
(**)
(*// GetStatusDescription Get space status description i.e. reserved, by who, when, etc.*)
(*// Returns empty string if space is free*)
(*func (p *Space) GetStatusDescription() string {*)
(*	status := ""*)
(*	if p.Reserved {*)
(*		// timeStr := p.ReservedTime.Format("Mon 15:04")*)
(*		// status = fmt.Sprintf("_:bust_in_silhouette:*<@%s>*\ton\t:clock1: *%s*_", p.ReservedById, timeStr)*)
(*		status = fmt.Sprintf("<@%s>", p.ReservedById)*)
(*	}*)
(*	return status*)
(*}*)
(**)
(*func (p *Space) Key() SpaceKey {*)
(*	return MakeSpaceKey(p.Number, p.Floor)*)
(*}*)
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
(*func MakeFloorStr(floor int) string {*)
(*	postfix := "th"*)
(**)
(*	postfixes := []string{"st", "nd", "rd"}*)
(*	absFloor := abs(floor)*)
(*	if 1 <= absFloor && absFloor <= 3 {*)
(*		postfix = postfixes[absFloor-1]*)
(*	}*)
(*	return fmt.Sprintf("%d%s floor", floor, postfix)*)
(*}*)
(**)
(*func MakeSpaceKey(number, floor int) SpaceKey {*)
(*	return SpaceKey(fmt.Sprintf("%s %d", MakeFloorStr(floor), number))*)
(*}*)

let%expect_test "props_txt" =
  let s = Space.make 1 4 "Verification Room" in
  printf "%s" (Space.props_txt s);
  [%expect {| (4 floor - Verification Room) |}]
