open Release
open Core

(*const defaultRingBufCapacity = 10*)
(*	ErrEmpty           = errors.New("empty")*)
(*	ErrNotFound        = errors.New("notFound")*)
(*	ErrOutOfRange      = errors.New("id out of range")*)
(*	ErrReleaseMismatch = errors.New("release mismatch")*)
(*type ReleasePool struct {*)
(*	Capacity int*)
(*	Data     []*ReleaseInfo*)
(*}*)
(**)
(*func NewReleasePoolWithCapacity(cap int) ReleasePool, error) {*)
(*	if cap <= 0 {*)
(*		return nil, fmt.Errorf("capacity must be > 0: %d", cap)*)
(*	}*)
(*	return &ReleasePool{*)
(*		Capacity: cap,*)
(*		Data:     make([]*ReleaseInfo, cap),*)
(*	}, nil*)
(*}*)
(**)
(*func NewReleasePool() *ReleasePool {*)
(*	p, _ := NewReleasePoolWithCapacity(defaultRingBufCapacity)*)
(*	return p*)
(*}*)
(**)

module ReleasePool = struct
  type t = { capacity : int; data : ReleaseInfo.t option array }
  [@@deriving show]

  let make ?(capacity = 10) () =
    (*TODO: add error handling for capacity*)
    { capacity; data = Array.create ~len:capacity None }

  (*TODO: fix this, its not working*)
  let grow p =
    let cap = p.capacity in
    Array.blit ~src:p.data ~src_pos:(cap - 1)
      ~dst:(Array.create ~len:cap None)
      ~dst_pos:cap ~len:cap;
    { p with capacity = 2 * cap }

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
end

let%expect_test "find_free.all_empty" =
  let p = ReleasePool.make () in
  let index = ReleasePool.find_free p in

  let out = [%message "" (index : int option)] |> Sexp.to_string_hum in
  printf "%s" out;
  [%expect {| (index (0)) |}]

let%expect_test "find_free.first_taken" =
  let p = ReleasePool.make ~capacity:3 () in
  let new_data = [| Some Release.make_test_release; None; None |] in
  let p = { p with data = new_data } in

  let index = ReleasePool.find_free p in

  let out = [%message "" (index : int option)] |> Sexp.to_string_hum in
  printf "%s" out;
  [%expect {| (index (1)) |}]

let%expect_test "find_free.all_taken" =
  let p = ReleasePool.make ~capacity:3 () in
  let test_release = Release.make_test_release in
  let new_data =
    [| Some test_release; Some test_release; Some test_release |]
  in
  let p = { p with data = new_data } in

  let index = ReleasePool.find_free p in

  let out = [%message "" (index : int option)] |> Sexp.to_string_hum in
  printf "%s" out;
  [%expect {| (index ()) |}]

let%expect_test "put.without_growing" =
  let p = ReleasePool.make ~capacity:3 () in
  let test_release = Release.make_test_release in

  let p = ReleasePool.put p test_release in

  printf "%s" (ReleasePool.show p);
  [%expect
    {|
    { Release_pool.ReleasePool.capacity = 3;
      data =
      [|(Some { Release.ReleaseInfo.releaser_id = "RELEASER_ID";
                owner_id = "OWNER_ID"; owner_name = "OWNER_NAME";
                space_key = "-2nd floor 120"; start_date = <empty>;
                end_date = <empty>; cancelled = false; submitted = false;
                submitted_time = <empty>; unique_id = (Some 0); active = false;
                active_time = <empty>;
                created_time = 2025-05-14 17:30:09.422412902Z;
                root_view_id = None; view_id = None });
        None; None|]
      }
    |}];
  let p = ReleasePool.put p test_release in

  printf "%s" (ReleasePool.show p);
  [%expect
    {|
    { Release_pool.ReleasePool.capacity = 3;
      data =
      [|(Some { Release.ReleaseInfo.releaser_id = "RELEASER_ID";
                owner_id = "OWNER_ID"; owner_name = "OWNER_NAME";
                space_key = "-2nd floor 120"; start_date = <empty>;
                end_date = <empty>; cancelled = false; submitted = false;
                submitted_time = <empty>; unique_id = (Some 0); active = false;
                active_time = <empty>;
                created_time = 2025-05-14 17:30:09.422412902Z;
                root_view_id = None; view_id = None });
        (Some { Release.ReleaseInfo.releaser_id = "RELEASER_ID";
                owner_id = "OWNER_ID"; owner_name = "OWNER_NAME";
                space_key = "-2nd floor 120"; start_date = <empty>;
                end_date = <empty>; cancelled = false; submitted = false;
                submitted_time = <empty>; unique_id = (Some 1); active = false;
                active_time = <empty>;
                created_time = 2025-05-14 17:30:09.422412902Z;
                root_view_id = None; view_id = None });
        None|]
      }
    |}]

let%expect_test "find_free.all_taken" =
  let p = ReleasePool.make ~capacity:3 () in
  let test_release = Release.make_test_release in
  let new_data =
    [| Some test_release; Some test_release; Some test_release |]
  in
  let p = { p with data = new_data } in

  let p = ReleasePool.put p test_release in

  printf "%s" (ReleasePool.show p);
  [%expect {| |}]

(*func (p *ReleasePool) grow(new_size int) {*)
(*	// Reallocate the whole array with 2x cap*)
(*	new_data := make([]*ReleaseInfo, new_size)*)
(**)
(*	// Realign start to the beginning of the array*)
(*	n_copied := copy(new_data, p.Data)*)
(*	if n_copied != p.Capacity {*)
(*		log.Fatalf("copied %d but have %d data", n_copied, p.Capacity)*)
(*	}*)
(**)
(*	p.Data = new_data*)
(*	p.Capacity = new_size*)
(*}*)
(**)
(**)
(*// Remove replace the first element of pool that matches the provided*)
(*// value with an empty value*)
(*func (p *ReleasePool) Remove(id int) error {*)
(*	if id < 0 && id > p.Capacity {*)
(*		return fmt.Errorf *)
(*			"%w: can't remove release with id=%d from pool with size %d",*)
(*			ErrOutOfRange,*)
(*			id,*)
(*			p.Capacity,*)
(*		)*)
(*	}*)
(**)
(*	if p.Data[id] == nil {*)
(*		return fmt.Errorf *)
(*			"%w: can't remove release with id=%d from pool - no value at that idx",*)
(*			ErrEmpty,*)
(*			id,*)
(*		)*)
(*	}*)
(**)
(*	p.Data[id] = nil*)
(*	return nil*)
(*}*)
(**)
(*func (p *ReleasePool) ByIdx(id int) *ReleaseInfo {*)
(*	return p.Data[id]*)
(*}*)
(**)
(*func (p *ReleasePool) ByRootViewId(id string) *ReleaseInfo {*)
(*	for _, v := range p.Data {*)
(*		if v != nil && v.RootViewId == id {*)
(*			return v*)
(*		}*)
(*	}*)
(*	return nil*)
(*}*)
(**)
(*func (p *ReleasePool) ByViewId(id string) *ReleaseInfo {*)
(*	for _, v := range p.Data {*)
(*		if v != nil && v.ViewId == id {*)
(*			return v*)
(*		}*)
(*	}*)
(*	return nil*)
(*}*)
(**)
(*func (p *ReleasePool) All() []*ReleaseInfo {*)
(*	var releases []*ReleaseInfo*)
(**)
(*	for _, release := range p.Data {*)
(*		if release == nil {*)
(*			continue*)
(*		}*)
(**)
(*		releases = append(releases, release)*)
(*	}*)
(**)
(*	return releases*)
(*}*)
(**)
(*func (p *ReleasePool) Active() *ReleaseInfo {*)
(*	for _, v := range p.Data {*)
(*		if v != nil && v.Active {*)
(*			return v*)
(*		}*)
(*	}*)
(*	return nil*)
(*}*)
