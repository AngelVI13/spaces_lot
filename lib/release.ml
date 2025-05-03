open Core

module ReleaseInfo = struct
  type t = {
    releaser_id : string;
    owner_id : string;
    owner_name : string;
    space_key : Space_key.t;
    start_date : Date.t option; [@printer Utils.pp_date_opt]
    end_date : Date.t option; [@printer Utils.pp_date_opt]
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

  let shout fmt = Printf.ksprintf (fun s -> s ^ "!") fmt

  let mark_submitted r ~releaser =
    Log.info "ReleaseInfo Submitted: releaser=%s; info=%s" releaser (show r);
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

  (** Checks if data is present in the release. Fields like `releaser_id`,
      `owner_id`, `owner_name` and `space_key` are required during release
      creation so they are not checked here *)
  let is_data_present r =
    String.(r.releaser_id <> "")
    && String.(r.owner_id <> "")
    && String.(r.owner_name <> "")
    && Option.is_some r.start_date
    && Option.is_some r.end_date

  (** Checks if release dates are present and valid i.e. today <= start_date <=
      end_date *)
  let is_valid r =
    if not (is_data_present r) then
      Or_error.error_s
        [%message
          "Missing date information for temporary release of space"
            (r.space_key : Space_key.t)
            (r.start_date : _ option)
            (r.end_date : _ option)]
    else
      let today = Utils.today_date () in
      let start_date, end_date =
        match (r.start_date, r.end_date) with
        | Some s, Some e -> (s, e)
        | _ -> assert false
      in
      Utils.is_date_range_valid ~today ~start_date ~end_date

  let show_custom r =
    (*NOTE: this uses https://github.com/janestreet/ppx_sexp_message*)
    [%message
      "ReleaseInfo"
        ~space:(r.space_key : Space_key.t)
        ~userName:(r.owner_name : string)
        (r.start_date : Date.t option)
        (r.end_date : Date.t option)
        ~id:(r.unique_id : int option)]
    |> Sexp.to_string_hum

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
end

let print_is_valid_result r =
  let res =
    match ReleaseInfo.is_valid r with
    | Ok _ -> sprintf "Release is valid"
    | Error e -> sprintf "Release is not valid: %s" (Error.to_string_hum e)
  in
  print_string res

let make_test_release =
  let space_key = Space_key.make ~floor:(-2) ~number:120 in
  ReleaseInfo.make ~root_view_id:None ~releaser_id:"RELEASER_ID"
    ~owner_id:"OWNER_ID" ~owner_name:"OWNER_NAME" ~space_key

let%expect_test "release.show_custom" =
  let r = make_test_release in
  printf "%s" (ReleaseInfo.show_custom r);
  [%expect
    {|
    (ReleaseInfo (space "-2nd floor 120") (userName OWNER_NAME) (r.start_date ())
     (r.end_date ()) (id ()))
    |}]

let%expect_test "release.is_valid.missing_start_end" =
  let r = make_test_release in
  print_is_valid_result r;
  [%expect
    {|
    Release is not valid: ("Missing date information for temporary release of space"
     (r.space_key "-2nd floor 120") (r.start_date ()) (r.end_date ()))
    |}]

let%expect_test "release.is_valid.dates_present_but_range_incorrect" =
  let r = make_test_release in
  let r =
    {
      r with
      start_date = Some (Date.of_string "2025-04-27");
      end_date = Some (Date.of_string "2025-04-26");
    }
  in
  print_is_valid_result r;
  [%expect
    {| Release is not valid: ("Start date is in the past" (start_str 2025-04-27)) |}]

(*TODO: this test is bad because the `ReleaseInfo.is_valid` is based on
  today's date. Thats why I'm using dates in the far future to test this*)
let%expect_test "release.is_valid" =
  let r = make_test_release in
  let r =
    {
      r with
      start_date = Some (Date.of_string "2029-04-26");
      end_date = Some (Date.of_string "2029-04-28");
    }
  in
  print_is_valid_result r;
  [%expect {| Release is valid |}]
