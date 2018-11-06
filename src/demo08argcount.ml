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

class ['a, 'b] show_tpairint  fa fb fself =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_tpairint
    inherit [('a * 'b)] show_toption
        (fun (x,y) -> sprintf "(%s,%s)" (fa x) (fb y))
        fself
  end

class ['a, 'a1, 'b, 'b1] gmap_tpairint  fa fb fself =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) tpairint] class_tpairint
    inherit [('a * 'b), ('a1 * 'b1)] gmap_toption
        (fun (x, y) -> (fa x, fb y))
        fself
  end

let gcata_tpairint = gcata_toption
let t = { gcata = gcata_tpairint; plugins = object end }


let show_tpairint fa fb s = transform(t) (new show_tpairint fa fb) s
let gmap_tpairint fa fb s = transform(t) (new gmap_tpairint fa fb) s

let _ =
  Printf.printf "Original: %s\nMapped: %s\n"
    (show_tpairint show_int id (Some (1, "2")))
    (show_tpairint show_int id (gmap_tpairint (fun x -> x+1) id (Some (1, "2"))))

(* ****************************** Increasing a kind: more complex example  *)
type ('a, 'b) maybe_list_pairs = ('a * 'b) list option

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_maybe_list_pairs =
  object
    inherit [ ('a * 'b) list , ('ia * 'ib) list, ('sa * 'sb) list
            , 'inh, 'syn] class_toption
  end
class ['a, 'b] show_maybe_list_pairs fa fb fself = object
  inherit ['a, unit, string, 'b, unit, string, unit, string] class_maybe_list_pairs
  inherit [('a * 'b) list] show_toption
      (show_list (fun (x,y) -> sprintf "(%s,%s)" (fa x) (fb y)))
      fself
end

class ['a, 'a1, 'b, 'b1] gmap_maybe_list_pairs fa fb fself = object
  inherit [ 'a, unit, 'a1, 'b, unit, 'b1
          , unit, ('a1, 'b1) maybe_list_pairs] class_maybe_list_pairs
  inherit [('a * 'b) list, ('a1 * 'b1) list] gmap_toption
      (gmap_list (fun (x, y) -> (fa x, fb y)))
      (fun _ -> assert false)
end

let gcata_maybe_list_pairs = gcata_toption
let maybe_list_pairs = { gcata = gcata_maybe_list_pairs; plugins = object end }

let show_maybe_list_pairs fa fb s = transform(maybe_list_pairs)
    (new show_maybe_list_pairs fa fb) s
let gmap_maybe_list_pairs fa fb s = transform(maybe_list_pairs)
    (new gmap_maybe_list_pairs fa fb) s

let _ = printf "Original: %s\nMapped: %s\n"
    (show_maybe_list_pairs id string_of_int @@
     Some (Cons (("a", 1), Cons (("b",2), Nil))))

    (show_maybe_list_pairs id id @@
     gmap_maybe_list_pairs (fun x -> x ^ "!") (fun x -> sprintf "%d" (x*10)) @@
       Some (Cons (("a", 1), Cons (("b",2), Nil))))
