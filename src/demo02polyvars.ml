open Utils
open Printf

type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
and ('self,'a,'b) pv_open =
  'self constraint 'self = [> `A of 'a  | `B of 'b ]


class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'polyvar_extra, 'inh, 'syn] class_pv =
  object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end

class ['a, 'b] show_pv self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, 'polyvar_extra, unit, string] class_pv
    method c_A () a = sprintf "`A (%a)" fa a
    method c_B () b = sprintf "`B (%a)" fb b
  end

(* I added an extra argument for the 'self variable in the open type
 * We need this in gmap class and not in a show implementation
*)
class ['a, 'a1, 'b, 'b1, 'polyvar_extra] map_pv
    self (fa : unit -> 'a -> 'a1) (fb : unit -> 'b -> 'b1) =
  object
    inherit [ 'a, unit, 'a1, 'b, unit, 'b1
            , 'polyvar_extra, unit, 'polyvar_extra] class_pv
    method c_A () a = `A (fa () a)
    method c_B () b = `B (fb () b)
  end

let rec gcata_pv tr inh p =
  match p with
  | `A a -> tr#c_A inh a
  | `B b -> tr#c_B inh b

let show_pv fa fb t = fix0 (fun self t -> gcata_pv (new show_pv self (fun () -> fa) (fun () -> fb)) () t) t
let gmap_pv fa fb t = fix0 (fun self t -> gcata_pv (new map_pv  self (fun () -> fa) (fun () -> fb)) () t) t

let _ =
  Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (show_pv id            id (`A "1"))
      (show_pv string_of_int id @@
       gmap_pv int_of_string id (`A "1"))


(* ********************************************************************************************** *)
type ('a, 'b) pv_ext = [ `C of 'a | ('a, 'b) pv ]
and ('self,'a,'b) pv_ext_open =
  'self constraint 'self = [> `C of 'a  | ('a, 'b) pv ]

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'polyvar_extra, 'inh, 'syn] class_pv_ext =
  object
    inherit ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'polyvar_extra, 'inh, 'syn] class_pv
    method virtual c_C : 'inh -> 'a -> 'syn
  end
class ['a, 'b] show_pv_ext self fa fb =
  object
    inherit [ 'a, unit, string, 'b, unit, string
            , ('a, 'b) pv_ext, unit, string] class_pv_ext
    inherit ['a, 'b] show_pv self fa fb
    method c_C () a = sprintf "`A (%a)" fa a
  end

class ['a, 'a1, 'b, 'b1, 'polyvar_extra] map_pv_ext
          self (fa : unit -> 'a -> 'a1) (fb : unit -> 'b -> 'b1) =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1
            , 'polyvar_extra, unit, ('polyvar_extra, 'a1, 'b1) pv_ext_open
            ] class_pv_ext
    inherit ['a, 'a1, 'b, 'b1, 'polyvar_extra ] map_pv self fa fb
    method c_C () a = `C (fa () a)
  end

let rec gcata_pv_ext tr inh p =
  match p with
  | `C a -> tr#c_C inh a
  | #pv as subj -> gcata_pv tr inh subj

let show_pv_ext fa fb t =
  fix0 (fun self t -> gcata_pv_ext
    (new show_pv_ext self (fun () -> fa) (fun () -> fb)) () t) t

let gmap_pv_ext fa fb t = fix0 (fun self t ->
    gcata_pv_ext (new map_pv_ext (fun () -> self) (fun () -> fa) (fun () -> fb)) () t
  ) t

let _ =
  Printf.printf "Original pv: %s\n" @@
      show_pv  id            id (`A "1");
  Printf.printf "Mapped pv and showed as a pv_ext: %s\n" @@
    show_pv_ext string_of_int id
      (gmap_pv int_of_string id (`A "1"));
  Printf.printf "Original pv_ext: %s\n" @@
    show_pv_ext id            id (`C "1");
  Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n" @@
    show_pv_ext string_of_int  id
      (gmap_pv_ext int_of_string id (`C "1"));
