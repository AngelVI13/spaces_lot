open Release
open Core
open Poly

(*const defaultRingBufCapacity = 10*)
(*	ErrEmpty           = errors.New("empty")*)
(*	ErrNotFound        = errors.New("notFound")*)
(*	ErrOutOfRange      = errors.New("id out of range")*)
(*	ErrReleaseMismatch = errors.New("release mismatch")*)
(**)
(*	if cap <= 0 {*)
(*		return nil, fmt.Errorf("capacity must be > 0: %d", cap)*)
(*	}*)

module ReleasePool = struct
  type t = { capacity : int; data : ReleaseInfo.t option array }
  [@@deriving show]

  let make ?(capacity = 10) () =
    (*TODO: add error handling for capacity*)
    { capacity; data = Array.create ~len:capacity None }

  let grow p =
    let new_cap = 2 * p.capacity in
    let new_data = Array.create ~len:new_cap None in
    Array.blit ~src:p.data ~src_pos:0 ~dst:new_data ~dst_pos:0 ~len:p.capacity;
    { capacity = new_cap; data = new_data }

  (** Returns the index of the first free space of the pool *)
  let find_free p =
    match Array.findi ~f:(fun _ value -> Option.is_none value) p.data with
    | None -> None
    | Some (i, _) -> Some i

  let put p release =
    let p, idx =
      match find_free p with
      | None -> (
          let p = grow p in
          match find_free p with None -> assert false | Some i -> (p, i))
      | Some i -> (p, i)
    in
    let release = ReleaseInfo.with_id release (Some idx) in
    Array.set p.data idx (Some release);
    p

  let get_by_idx p idx =
    match idx with
    | idx when idx >= 0 && idx < p.capacity -> Ok (Array.get p.data idx)
    | idx ->
        Or_error.error_s
          [%message "idx out of range" (idx : int) (p.capacity : int)]

  (** Remove replaces the release on that idx with None *)
  let remove p idx =
    let open Or_error.Monad_infix in
    get_by_idx p idx >>= fun value ->
    match value with
    | Some _ ->
        Array.set p.data idx None;
        Ok ()
    | None ->
        Or_error.error_s
          [%message "Can't remove release - no value at that idx" (idx : int)]

  let show_cap_and_ids p =
    sprintf "cap=%d data=[%s]" p.capacity
      (Array.fold ~init:""
         ~f:(fun acc el ->
           match el with
           | None -> sprintf "%s; None" acc
           | Some r ->
               sprintf "%s; %d" acc (Option.value ~default:(-1) r.unique_id))
         p.data)

  (*TODO: test all of the following methods*)

  (*TODO: the following 2 are identical except the field they are checking - parameterize somehow?*)
  let get_by_root_view_id p root_view_id =
    let filtered =
      Array.filter
        ~f:(fun el ->
          match el with
          | None -> false
          | Some el -> Option.value ~default:"" el.root_view_id = root_view_id)
        p.data
    in
    match filtered with [||] -> None | _ -> filtered.(0)

  let get_by_view_id p view_id =
    let filtered =
      Array.filter
        ~f:(fun el ->
          match el with
          | None -> false
          | Some el -> Option.value ~default:"" el.view_id = view_id)
        p.data
    in
    match filtered with [||] -> None | _ -> filtered.(0)

  let all p =
    Array.fold ~init:[]
      ~f:(fun acc el -> match el with None -> acc | Some el -> el :: acc)
      p.data

  let active p =
    let filtered =
      Array.filter
        ~f:(fun el -> match el with None -> false | Some el -> el.active)
        p.data
    in
    match filtered with [||] -> None | _ -> filtered.(0)
end

let%expect_test "find_free.all_empty" =
  let p = ReleasePool.make () in
  let index = ReleasePool.find_free p in

  [%message "" (index : int option)] |> Sexp.to_string_hum |> printf "%s";
  [%expect {| (index (0)) |}]

let%expect_test "find_free.first_taken" =
  let p = ReleasePool.make ~capacity:3 () in
  let new_data = [| Some Release.make_test_release; None; None |] in
  let p = { p with data = new_data } in

  let index = ReleasePool.find_free p in

  [%message "" (index : int option)] |> Sexp.to_string_hum |> printf "%s";
  [%expect {| (index (1)) |}]

let%expect_test "find_free.all_taken" =
  let p = ReleasePool.make ~capacity:3 () in
  let test_release = Release.make_test_release in
  let new_data =
    [| Some test_release; Some test_release; Some test_release |]
  in
  let p = { p with data = new_data } in

  let index = ReleasePool.find_free p in

  [%message "" (index : int option)] |> Sexp.to_string_hum |> printf "%s";
  [%expect {| (index ()) |}]

let%expect_test "put.without_growing" =
  let p = ReleasePool.make ~capacity:3 () in
  let test_release = Release.make_test_release in

  let p = ReleasePool.put p test_release in

  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=3 data=[; 0; None; None] |}];
  let p = ReleasePool.put p test_release in

  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=3 data=[; 0; 1; None] |}]

let%expect_test "put.with_growing" =
  let p = ReleasePool.make ~capacity:2 () in
  let test_release = Release.make_test_release in
  let p = ReleasePool.put p test_release in
  let p = ReleasePool.put p test_release in
  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=2 data=[; 0; 1] |}];

  let p = ReleasePool.put p test_release in

  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=4 data=[; 0; 1; 2; None] |}]

let%expect_test "remove.existing_id" =
  let p = ReleasePool.make ~capacity:2 () in
  let test_release = Release.make_test_release in
  let p = ReleasePool.put p test_release in
  let p = ReleasePool.put p test_release in
  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=2 data=[; 0; 1] |}];

  let res = ReleasePool.remove p 1 in
  printf "%b" (Result.is_ok res);
  [%expect {| true |}];
  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=2 data=[; 0; None] |}]

let%expect_test "remove.existing_id_but_empty" =
  let p = ReleasePool.make ~capacity:2 () in
  let test_release = Release.make_test_release in

  let p = ReleasePool.put p test_release in

  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=2 data=[; 0; None] |}];

  let res = ReleasePool.remove p 1 in

  [%message "" ~_:(res : (unit, Error.t) result)]
  |> Sexp.to_string_hum |> printf "%s";
  [%expect
    {| (Error ("Can't remove release - no value at that idx" (idx 1))) |}];

  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=2 data=[; 0; None] |}]

let%expect_test "remove.non_existing_id" =
  let p = ReleasePool.make ~capacity:2 () in

  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=2 data=[; None; None] |}];

  let res = ReleasePool.remove p 3 in

  [%message "" ~_:(res : (unit, Error.t) result)]
  |> Sexp.to_string_hum |> printf "%s";
  [%expect {| (Error ("idx out of range" (idx 3) (p.capacity 2))) |}];

  print_string (ReleasePool.show_cap_and_ids p);
  [%expect {| cap=2 data=[; None; None] |}]

let%expect_test "get_by_idx.existing_id" =
  let p = ReleasePool.make ~capacity:2 () in
  let test_release = Release.make_test_release in
  let p = ReleasePool.put p test_release in
  let p = ReleasePool.put p test_release in

  let res = ReleasePool.get_by_idx p 1 in
  printf "%s"
    (match res with
    | Error e -> Error.to_string_hum e
    | Ok res -> (
        match res with
        | None -> "None"
        | Some res -> sprintf "%d" (Option.value ~default:(-1) res.unique_id)));
  [%expect {| 1 |}]

let%expect_test "get_by_idx.non_existing_id" =
  let p = ReleasePool.make ~capacity:2 () in
  let test_release = Release.make_test_release in
  let p = ReleasePool.put p test_release in
  let p = ReleasePool.put p test_release in

  let res = ReleasePool.get_by_idx p 3 in
  printf "%s"
    (match res with
    | Error e -> Error.to_string_hum e
    | Ok res -> (
        match res with
        | None -> "None"
        | Some res -> sprintf "%d" (Option.value ~default:(-1) res.unique_id)));
  [%expect {| ("idx out of range" (idx 3) (p.capacity 2)) |}]
