
type ('a, 'b) alist = Nil | Cons of 'a * 'b

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_alist =
  object
    method virtual c_Nil  : 'inh -> 'syn
    method virtual c_Cons : 'inh -> 'a -> 'b -> 'syn
  end

class ['a, 'b] show_alist self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_alist
    method c_Nil  _     = "[]"
    method c_Cons _ a b = Printf.sprintf "%s :: %s" (fa () a) (fb () b)
  end
class ['a, 'a1, 'b, 'b1] map_alist self fa fb =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) alist] class_alist
    method c_Nil  _     = Nil
    method c_Cons _ a b = Cons (fa () a, fb () b)
  end

let rec gcata_alist tr inh t =
  match t with
  | Nil         -> tr#c_Nil inh
  | Cons (a, b) -> tr#c_Cons inh a b

type 'a list = ('a, 'a list) alist

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_list =
  object
    inherit ['a, 'is, 'sa, 'a list, 'ia list, 'sa list, 'inh, 'syn] class_alist
  end

class ['a] show_list self fa =
  object
    inherit ['a, unit, string, unit, string] class_list
    inherit ['a, 'a list] show_alist self fa self
  end
class ['a, 'b] map_list self fa =
  object
    inherit ['a, unit, 'b, unit, 'b list] class_list
    inherit ['a, 'b, 'a list, 'b list] map_alist self fa self
  end
let rec gcata_list tr inh t = gcata_alist tr inh t


(* Fix 1: many allocations of objects *)
let fix1 f t =
  let knot = ref (fun _ -> assert false) in
  let recurse t = f !knot t in
  knot := recurse;
  recurse t

let map_list1 fa t =
  fix1 (fun self ->
    gcata_list (new map_list self (fun () -> fa))
  ) () t

(* Fix 2: with lazy *)
let map_list2 fa xs =
  let rec helper =
    let mem = lazy (gcata_list @@ new map_list helper (fun () -> fa)) in
    fun () xs -> (Lazy.force mem) () xs
  in
  helper () xs

(* Fix 3: manual check of if we have entered there *)
let map_list3 fa xs =
  let save = ref None in
  fix1 (fun self ->
      match !save with
      | None ->
        let ans = gcata_list (new map_list self (fun () -> fa)) in
        let () = save := Some ans in
        ans
      | Some ans -> ans
  ) () xs

(* Attempt 4 *)
let fix2 f subj =
  let fself_holder = ref (fun _ -> failwith "self-transfomration called too early") in
  let rec knot subj = !fself_holder subj in
  let fself = f knot in
  (* actual transformation was not yet executed *)
  fself_holder := fself;
  !fself_holder subj

let map_list4 fa t =
  fix2 (fun self ->
    gcata_list (new map_list self (fun () -> fa))
  ) () t

let show_list4 fa t =
  fix2 (fun self ->
      (* let _ = ignore @@ self () Nil in (* crashes *) *)
      gcata_list (new show_list self (fun () -> fa))
  ) () t

open Core
open Core_bench.Std

let make_list size =
  let rec helper acc = function
    | 0 -> acc
    | n -> helper (Cons (n, acc)) (n-1)
  in
  helper Nil size

let lst0 = make_list 10

let () =
  print_endline @@ show_list4 string_of_int lst0;
  print_endline @@ show_list4 string_of_int @@ map_list4 ((+)1) lst0

let lst1 = make_list 1000

let main () =
  Random.self_init ();
  (* let x = Random.float 10.0 in
   * let y = Random.float 10.0 in *)

  Command.run (Bench.make_command [
    Bench.Test.create ~name:"Naive with many allocations" (fun () ->
      ignore (map_list1 ((+)1) lst1) );
    Bench.Test.create ~name:"With lazy" (fun () ->
      ignore (map_list2 ((+)1) lst1) );
    Bench.Test.create ~name:"Outer mutable field" (fun () ->
      ignore (map_list3 ((+)1) lst1) );
    Bench.Test.create ~name:"wtf" (fun () ->
      ignore (map_list4 ((+)1) lst1) );
  ])

let () = main ()
