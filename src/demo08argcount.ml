open Utils
open Demo04option
open Demo07lists
open Printf

(* --------- Increasing the kind -------------- *)

type ('a, 'b) tpairint = ('a * 'b) option

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_tpairint =
  object
    inherit [('a * 'b), ('ia * 'ib), ('sa * 'sb), 'inh, 'syn] class_toption
  end

class ['a, 'b] show_tpairint self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_tpairint
    inherit [('a * 'b)] show_toption self
        (fun () (x,y) -> sprintf "(%a,%a)" fa x fb y)
  end

class ['a, 'a1, 'b, 'b1] map_tpairint self fa fb =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) tpairint] class_tpairint
    inherit [('a * 'b), ('a1 * 'b1)] map_toption self
        (fun () (x, y) -> (fa () x, fb () y))
  end

let gcata_t : ('a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn) #class_tpairint -> 'inh -> ('a, 'b) tpairint -> 'syn =
  fun tr inh t -> gcata_toption tr inh t

let gcata_tpairint = gcata_t

let show_tpairint fa fb t = fix0 (fun self t ->
    gcata_tpairint (new show_tpairint self (fun () -> fa) (fun () -> fb)) () t
  ) t
let gmap_tpairint fa fb t = fix0 (fun self t ->
    gcata_tpairint (new map_tpairint self (fun () -> fa) (fun () -> fb)) () t
  ) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n"
    (show_tpairint string_of_int id (Some (1, "2")))
    (show_tpairint string_of_int id (gmap_tpairint (fun x -> x+1) id (Some (1, "2"))))

(* ****************************** Increasing a kind: more complex example  *)
type ('a, 'b) maybe_list_pairs = ('a * 'b) list option

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_maybe_list_pairs =
  object
    inherit [ ('a * 'b) list , ('ia * 'ib) list, ('sa * 'sb) list
            , 'inh, 'syn] class_toption
  end
class ['a, 'b] show_maybe_list_pairs self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_maybe_list_pairs
    inherit [('a * 'b) list] show_toption self
        (fun () -> show_list (fun (x,y) -> sprintf "(%a,%a)" fa x fb y))
  end

class ['a, 'a1, 'b, 'b1] map_maybe_list_pairs self fa fb =
  object
    inherit [ 'a, unit, 'a1, 'b, unit, 'b1
            , unit, ('a1, 'b1) maybe_list_pairs] class_maybe_list_pairs
    inherit [('a * 'b) list, ('a1 * 'b1) list] map_toption (fun _ -> assert false)
        (fun () -> map_list (fun (x, y) -> (fa () x, fb () y)))
  end

let gcata_maybe_list_pairs = gcata_toption

let show_maybe_list_pairs fa fb t = fix0 (fun self t ->
    gcata_toption (new show_maybe_list_pairs self (fun () -> fa) (fun () -> fb)) () t
  ) t

let gmap_maybe_list_pairs fa fb t = fix0 (fun self t ->
    gcata_toption (new map_maybe_list_pairs self (fun () -> fa) (fun () -> fb)) () t
  ) t

let _ = printf "Original: %s\nMapped: %s\n"
    (show_maybe_list_pairs id string_of_int @@
     Some (Cons (("a", 1), Cons (("b",2), Nil))))

    (show_maybe_list_pairs id id @@
     gmap_maybe_list_pairs (fun x -> x ^ "!") (fun x -> sprintf "%d" (x*10)) @@
       Some (Cons (("a", 1), Cons (("b",2), Nil))))
