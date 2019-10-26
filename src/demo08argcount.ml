open Utils
open Demo04option
open Demo07lists
open Printf

(* --------- Increasing the kind -------------- *)

type ('a, 'b) tpairint = ('a * 'b) option

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'syn] class_tpairint =
  object
    inherit [ ('ia * 'ib), ('a * 'b), ('sa * 'sb), 'inh, 'syn] class_toption
  end

class ['a, 'b] show_tpairint  fa fb fself =
  object
    inherit [ unit, 'a, string
            , unit, 'b, string, unit, string] class_tpairint
    inherit [('a * 'b)] show_toption
        (fun () (x,y) -> sprintf "(%s,%s)" (fa () x) (fb () y))
        fself
  end

class ['a, 'a1, 'b, 'b1] gmap_tpairint fa fb fself =
  object
    inherit [unit, 'a, 'a1, unit, 'b, 'b1, unit, ('a1, 'b1) tpairint] class_tpairint
    inherit [('a * 'b), ('a1 * 'b1)] gmap_toption
        (fun () (x, y) -> (fa () x, fb () y))
        fself
  end

let gcata_tpairint = gcata_toption
let t = { gcata = gcata_tpairint
        ; fix = (fun c -> GT.transform_gc gcata_tpairint c)
        }


let show_tpairint fa fb s = GT.transform(t) (new show_tpairint fa fb) () s
let gmap_tpairint fa fb s = GT.transform(t) (new gmap_tpairint fa fb) () s

let _ =
  Printf.printf "Original: %s\n"
    (show_tpairint (lift show_int) (lift id) (Some (1, "2")));
  Printf.printf "Original: %s\nMapped: %s\n" @@
  show_tpairint (lift show_int) (lift id) @@
  gmap_tpairint (fun () x -> x+1) (lift id) (Some (1, "2"))

(* ****************************** Increasing a kind: more complex example  *)
type ('a, 'b) maybe_list_pairs = ('a * 'b) list option

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'syn] class_maybe_list_pairs =
  object
    inherit [ ('ia * 'ib) list, ('a * 'b) list , ('sa * 'sb) list
            , 'inh, 'syn
            ] class_toption
  end
class ['a, 'b] show_maybe_list_pairs fa fb fself = object
  inherit [ unit, 'a, string, unit, 'b, string, unit, string] class_maybe_list_pairs
  inherit [('a * 'b) list] show_toption
      (fun () -> show_list (fun () (x,y) -> sprintf "(%a,%a)" fa x fb y))
      fself
end

class ['a, 'a1, 'b, 'b1] gmap_maybe_list_pairs fa fb fself = object
  inherit [  unit, 'a, 'a1
          ,  unit, 'b, 'b1
          , unit, ('a1, 'b1) maybe_list_pairs] class_maybe_list_pairs
  inherit [('a * 'b) list, ('a1 * 'b1) list] gmap_toption
      (fun () -> gmap_list (fun () (x, y) -> (fa () x, fb () y)))
      (fun _ -> assert false)
end

let gcata_maybe_list_pairs = gcata_toption
let maybe_list_pairs = { gcata = gcata_maybe_list_pairs
                       ; fix = (fun c -> GT.transform_gc gcata_toption c)
                       }

let show_maybe_list_pairs fa fb s = GT.transform(maybe_list_pairs)
    (new show_maybe_list_pairs fa fb) () s
let gmap_maybe_list_pairs fa fb s = GT.transform(maybe_list_pairs)
    (new gmap_maybe_list_pairs fa fb) () s

let () =
  printf "Original: %s\n"
    (show_maybe_list_pairs (lift id) (lift string_of_int) @@
     Some (Cons (("a", 1), Cons (("b",2), Nil))));
  printf "Mapped: %s\n" @@
  show_maybe_list_pairs (lift id)  (lift id) @@
  gmap_maybe_list_pairs (fun () x -> x ^ "!") (fun () x -> sprintf "%d" (x*10)) @@
  Some (Cons (("a", 1), Cons (("b",2), Nil)))
