open Utils
open Printf

module PV : sig
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
  type ('self,'a,'b) pv_open =
    'self constraint 'self = [> `A of 'a  | `B of 'b ]

  class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] class_pv :
  object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end
  class ['a, 'b] show_pv :
    (unit -> [> ('a,'b) pv] -> string) ->
    (unit -> 'a -> string) ->
    (unit -> 'b -> string) ->
    object
      method c_A : unit -> 'a -> string
      method c_B : unit -> 'b -> string
    end
  class ['a, 'a1, 'b, 'b1, 'extra] map_pv :
    (unit -> [> ('a,'b) pv ] -> ('extra,'a1,'b1) pv_open) ->
    (unit -> 'a -> 'a1) ->
    (unit -> 'b -> 'b1) ->
    object
      method c_A : unit -> 'a -> 'extra
      method c_B : unit -> 'b -> 'extra
    end

  val gcata_pv :
    ('a,_,'syn, 'b,_,'syn, 'inh, 'syn, _) #class_pv ->
    'inh -> [< `A of 'a | `B of 'b ] -> 'syn
  val show_pv :
    ('a -> string) -> ('b -> string) -> ('a,'b) pv -> string

  val gmap_pv : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) pv -> ('b,'d) pv
end = struct
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
  type ('self,'a,'b) pv_open = 'self
    constraint 'self = [> `A of 'a  | `B of 'b ]

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'polyvar_extra] class_pv =
  object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end

class ['a, 'b] show_pv self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string, 'polyvar_extra] class_pv
    method c_A () a = sprintf "`A (%a)" fa a
    method c_B () b = sprintf "`B (%a)" fb b
  end

(* I added an extra argument for the 'self variable in the open type
 * We need this in gmap class and not in a show implementation
*)
class ['a, 'a1, 'b, 'b1, 'extra ] map_pv
    self (fa : unit -> 'a -> 'a1) (fb : unit -> 'b -> 'b1) =
  object
    inherit [ 'a, unit, 'a1, 'b, unit, 'b1
            , unit, [> ('a1,'b1) pv ] as 'extra, 'extra] class_pv
    method c_A () a = match `A (fa () a) with #pv as x -> x
    method c_B () b = `B (fb () b)
  end

(* let (_:int) = new map_pv (assert false) (assert false) (assert false) *)
let rec gcata_pv tr inh p =
  match p with
  | `A a -> tr#c_A inh a
  | `B b -> tr#c_B inh b

let show_pv fa fb t = fix0 (fun self t -> gcata_pv (new show_pv self (fun () -> fa) (fun () -> fb)) () t) t
let gmap_pv fa fb t = fix0 (fun self t -> gcata_pv (new map_pv  self (fun () -> fa) (fun () -> fb)) () t) t

end

let _ =
  let open PV in
  Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (show_pv id            id (`A "1"))
      (show_pv string_of_int id @@
       gmap_pv int_of_string id (`A "1"))


(* ********** 2nd declaration ************************************************************* *)
module PVExt : sig
  open PV
  type ('a, 'b) pv_ext = [ ('a,'b) pv | `C of 'a ]
  type ('self,'a,'b) pv_ext_open = 'self
    constraint 'self = [> `C of 'a  | ('a, 'b) pv ]

  class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'polyvar_extra] class_pv_ext :
    object
      inherit ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'polyvar_extra] class_pv
      method virtual c_C : 'inh -> 'a -> 'syn
  end
  class ['a, 'b] show_pv_ext :
    (unit -> [> ('a,'b) pv_ext ] -> string) ->
    (unit -> 'a -> string) ->
    (unit -> 'b -> string) ->
    object
      inherit ['a,'b] show_pv
      (* method c_A : unit -> 'a -> string
       * method c_B : unit -> 'b -> string *)
      method c_C : unit -> 'a -> string
    end
class ['a, 'a2, 'b, 'b2, 'extra] map_pv_ext :
  (unit -> [> ('a,'b) pv_ext ] -> ('extra, 'a2,'b2) pv_ext_open) ->
  (unit -> 'a -> 'a2) ->
  (unit -> 'b -> 'b2) ->
  object
    inherit ['a, 'a2, 'b, 'b2, 'extra ] map_pv
    method c_C : unit -> 'a -> ('extra, 'a2,'b2) pv_ext_open
  end

  val gcata_pv_ext :
    < ('a,_,'syn, 'b,_,'syn, 'inh, 'syn, _) class_pv; c_C : 'inh -> 'a -> 'syn; .. > ->
    'inh -> [< `C of 'a | ('a,'b) pv ] -> 'syn

val show_pv_ext : ('a -> string) -> ('b -> string) ->
  (* [> ('a,'b) pv | `C of 'a ] -> *)
  ('a,'b) pv_ext ->
  (* [> ('a,'b) pv_ext ] ->   (\* seems to be wrong *\) *)
  string

  val gmap_pv_ext :
    ('a -> 'a2) -> ('b -> 'b2) -> ('a,'b) pv_ext -> ('a2, 'b2) pv_ext
end = struct
  open PV
  type ('a, 'b) pv_ext = [ `C of 'a | ('a, 'b) pv ]
  type ('self,'a,'b) pv_ext_open = 'self
    constraint 'self = [> `C of 'a  | ('a, 'b) pv ]

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'polyvar_extra] class_pv_ext =
  object
    inherit ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'polyvar_extra] class_pv
    method virtual c_C : 'inh -> 'a -> 'syn
  end

class ['a, 'b] show_pv_ext
    (self: unit -> [> ('a,'b) pv_ext ] -> string)
    (fa: unit -> 'a -> string) fb =
  object
    inherit [ 'a, unit, string, 'b, unit, string
            , unit, string
            , ('a, 'b) pv_ext
            ] class_pv_ext
    inherit ['a, 'b] show_pv self fa fb
    method c_C () a = sprintf "`C (%a)" fa a
  end

class ['a, 'a2, 'b, 'b2, 'extra] map_pv_ext
    (self: unit -> [> ('a,'b) pv_ext ] -> ('extra, 'a2,'b2) pv_ext_open)
    (fa : unit -> 'a -> 'a2)
    (fb : unit -> 'b -> 'b2) =
  object
    inherit ['a, unit, 'a2, 'b, unit, 'b2
            , unit, ('extra, 'a2,'b2) pv_ext_open
            , 'extra
            ] class_pv_ext
    inherit ['a, 'a2, 'b, 'b2, 'extra ] map_pv self fa fb
    method c_C () a : ('extra, 'a2,'b2) pv_ext_open =
      match `C (fa () a) with #pv_ext as x -> x
      (* `C (fa () a) *)
  end


let rec gcata_pv_ext tr inh p =
  match p with
  | `C a -> tr#c_C inh a
  | #pv as subj -> gcata_pv tr inh subj

(* let ___ fa fb self _:int = gcata_pv_ext
 *     (new show_pv_ext self (fun () -> fa) (fun () -> fb)) *)

let show_pv_ext fa fb t =
  fix0 (fun self t -> gcata_pv_ext
           (new show_pv_ext (fun () -> self) (fun () -> fa) (fun () -> fb)) () t) t

(* let (_:int)=  show_pv_ext *)
let gmap_pv_ext fa fb t =
  fix0 (fun self t -> gcata_pv_ext
           (new map_pv_ext  (fun () -> self) (fun () -> fa) (fun () -> fb)) () t) t

end

let _ =
  let open PV in
  let open PVExt in
  Printf.printf "****************************\n%!";
  Printf.printf "Original pv: %s\n" @@
      show_pv  id            id (`A "1");
  Printf.printf "Mapped pv and showed as a pv_ext: %s\n" @@
    show_pv_ext string_of_int id
      (gmap_pv int_of_string id (`A "1") :> (int,string) pv_ext);
  Printf.printf "Original pv_ext: %s\n" @@
    show_pv_ext id            id (`C "1");
  Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n" @@
    show_pv_ext string_of_int  id
      (gmap_pv_ext int_of_string id (`C "1"));


(* **************************************************************************************** *)
module PVExt2 : sig
  open PVExt
  type ('a, 'b) pv_ext2 = [ ('a,'b) pv_ext | `D of 'a ]
  type ('self,'a,'b) pv_ext2_open =
    'self constraint 'self = [> `C of 'a  | ('a, 'b) pv_ext2 ]

  class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] class_pv_ext2 :
  object
    inherit ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] class_pv_ext
    method virtual c_D : 'inh -> 'a -> 'syn
  end
  class ['a, 'b] show_pv_ext2 :
    (unit -> [> ('a,'b) pv_ext2 ] -> string) ->
    (unit -> 'a -> string) ->
    (unit -> 'b -> string) ->
    object
      inherit ['a,'b] show_pv_ext
      (* method c_A : unit -> 'a -> string
       * method c_B : unit -> 'b -> string *)
      method c_D : unit -> 'a -> string
    end
  class ['a, 'a2, 'b, 'b2, 'extra] map_pv_ext2 :
    (unit -> [> ('a,'b) pv_ext2 ] -> ('extra, 'a2,'b2) pv_ext2_open) ->
    (unit -> 'a -> 'a2) ->
    (unit -> 'b -> 'b2) ->
    object
      inherit ['a, 'a2, 'b, 'b2, 'extra ] map_pv_ext
      method c_D : unit -> 'a -> ('extra, 'a2,'b2) pv_ext2_open
    end

  val gcata_pv_ext2 :
    <  ('a,_,'syn, 'b,_,'syn, 'inh, 'syn, _) class_pv_ext
    ; c_D : 'inh -> 'a -> 'syn; .. > ->
    'inh -> [< ('a,'b) pv_ext | `D of 'a ] -> 'syn
  val show_pv_ext2 :
    ('a -> string) ->
    ('b -> string) -> ('a,'b) pv_ext2 -> string
  val gmap_pv_ext2 :
    ('a -> 'a2) ->
    ('b -> 'b2) ->
    ('a,'b) pv_ext2 -> ('a2,'b2) pv_ext2
end = struct
  open PVExt
  type ('a, 'b) pv_ext2 = [ ('a,'b) pv_ext | `D of 'a ]
  type ('self,'a,'b) pv_ext2_open = 'self
    constraint 'self = [> ('a, 'b) pv_ext | `D of 'a ]

  class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] class_pv_ext2 =
  object
    inherit ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn, 'extra] class_pv_ext
    method virtual c_D : 'inh -> 'a -> 'syn
  end

class ['a, 'b] show_pv_ext2
    (self: unit -> [> ('a,'b) pv_ext2 ] -> string)
    (fa: unit -> 'a -> string) fb =
  object
    inherit [ 'a, unit, string, 'b, unit, string
            , unit, string
            , ('a, 'b) pv_ext2
            ] class_pv_ext
    inherit ['a, 'b] show_pv_ext self fa fb
    method c_D () a = sprintf "`D (%a)" fa a
  end

class ['a, 'a2, 'b, 'b2, 'extra] map_pv_ext2
    (self: unit -> [> ('a,'b) pv_ext2 ] -> ('extra, 'a2,'b2) pv_ext2_open)
    (fa : unit -> 'a -> 'a2)
    (fb : unit -> 'b -> 'b2) =
  object
    inherit ['a, unit, 'a2, 'b, unit, 'b2
            , unit, ('extra, 'a2,'b2) pv_ext2_open
            , 'extra
            ] class_pv_ext2
    inherit ['a, 'a2, 'b, 'b2, 'extra ] map_pv_ext self fa fb
    method c_D () a : ('extra, 'a2,'b2) pv_ext2_open =
      match `D (fa () a) with #pv_ext2 as x -> x
      (* `C (fa () a) *)
  end


let rec gcata_pv_ext2 tr inh p =
  match p with
  | `D a -> tr#c_D inh a
  | #pv_ext as subj -> gcata_pv_ext tr inh subj

let show_pv_ext2 fa fb t =
  fix0 (fun self t -> gcata_pv_ext2
           (new show_pv_ext2 (fun () -> self) (fun () -> fa) (fun () -> fb)) () t) t

let gmap_pv_ext2 fa fb t = fix0 (fun self t ->
    gcata_pv_ext2 (new map_pv_ext2 (fun () -> self) (fun () -> fa) (fun () -> fb)) () t
  ) t

end

let _ =
  let open PVExt in
  let open PVExt2 in

  Printf.printf "****************************\n%!";
  Printf.printf "Original pv_ext: %s\n" @@
      show_pv_ext  id            id (`A "1");
  Printf.printf "Mapped pv_ext and showed as a pv_ext2: %s\n" @@
    show_pv_ext2 string_of_int id
      (gmap_pv_ext int_of_string id (`A "1") :> (int,string) pv_ext2);
  Printf.printf "Original pv_ext2: %s\n" @@
    show_pv_ext2 id            id (`D "1");
  Printf.printf "Mapped PV_ext2 and showed as a pv_ext2: %s\n" @@
    show_pv_ext2 string_of_int  id
      (gmap_pv_ext2 int_of_string id (`D "1"));
