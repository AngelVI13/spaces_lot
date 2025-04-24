open Core
open Space

module ReleaseInfo = struct
  type t = {
    releaser_id : string;
    owner_id : string;
    owner_name : string;
    space_key : SpaceKey.t;
    start_date : Time_ns.t option; [@printer Utils.pp_time_opt]
    end_date : Time_ns.t option; [@printer Utils.pp_time_opt]
    cancelled : bool;
    submitted : bool;
    submitted_time : Time_ns.t option; [@printer Utils.pp_time_opt]
    unique_id : int option;
    active : bool;
    active_time : Time_ns.t option; [@printer Utils.pp_time_opt]
    created_time : Time_ns.t; [@printer Utils.pp_time]
    (* These are only used while the user is choosing date range to refer*)
    (* between space selected and release range selected (i.e. between booking modal*)
    (* and corresponding release modal)*)
    root_view_id : string option;
    view_id : string option;
  }
  [@@deriving show]

  let make ~root_view_id ~releaser_id ~owner_id ~owner_name ~space_key =
    {
      releaser_id;
      owner_id;
      owner_name;
      space_key;
      start_date = None;
      end_date = None;
      cancelled = false;
      submitted = false;
      submitted_time = None;
      unique_id = None;
      active = false;
      active_time = None;
      created_time = Time_ns.now ();
      root_view_id;
      view_id = None;
    }

  let mark_submitted r ~releaser =
    (*TODO: how to do logging?*)
    printf "ReleaseInfo Submitted: releaser=%s; info=%s" releaser (show r);
    (* Need to reset view IDs as they are no longer needed.*)
    (* If we don't reset them and user tries to release another*)
    (* space without closing the parent model -> GetByViewId can return*)
    (* incorrect data.*)
    {
      r with
      submitted = true;
      submitted_time = Some (Time_ns.now ());
      root_view_id = None;
      view_id = None;
    }

  let mark_active r =
    printf "ReleaseInfo Active: info=%s" (show r);
    { r with active = true; active_time = Some (Time_ns.now ()) }

  let mark_cancelled r =
    printf "ReleaseInfo Cancelled: info=%s" (show r);
    { r with cancelled = true }
end

(*func (i *ReleaseInfo) DataPresent() bool {*)
(*	return (i.ReleaserId != "" &&*)
(*		i.OwnerId != "" &&*)
(*		i.OwnerName != "" &&*)
(*		i.Space != nil &&*)
(*		i.StartDate != nil &&*)
(*		i.EndDate != nil)*)
(*}*)
(**)
(*func (i *ReleaseInfo) Check() string {*)
(*	if !i.DataPresent() {*)
(*		return fmt.Sprintf*)
(*			"Missing date information for temporary release of space (%s)",*)
(*			i.Space.Key(),*)
(*		)*)
(*	}*)
(**)
(*	return common.CheckDateRange(i.StartDate, i.EndDate)*)
(*}*)
(**)
(*func (i ReleaseInfo) String() string {*)
(*	startDateStr := "nil"*)
(*	if i.StartDate != nil {*)
(*		startDateStr = i.StartDate.Format("2006-01-02")*)
(*	}*)
(**)
(*	endDateStr := "nil"*)
(*	if i.EndDate != nil {*)
(*		endDateStr = i.EndDate.Format("2006-01-02")*)
(*	}*)
(*	return fmt.Sprintf *)
(*		"ReleaseInfo(space=%s, userName=%s, start=%s, end=%s, id=%d)",*)
(*		i.Space.Key(),*)
(*		i.OwnerName,*)
(*		startDateStr,*)
(*		endDateStr,*)
(*		i.UniqueId,*)
(*	)*)
(*}*)
(**)
(*func (i ReleaseInfo) DateRange() string {*)
(*	startDateStr := "nil"*)
(*	if i.StartDate != nil {*)
(*		startDateStr = i.StartDate.Format("2006-01-02")*)
(*	}*)
(**)
(*	endDateStr := "nil"*)
(*	if i.EndDate != nil {*)
(*		endDateStr = i.EndDate.Format("2006-01-02")*)
(*	}*)
(*	return fmt.Sprintf("%s -> %s", startDateStr, endDateStr)*)
(*}*)
