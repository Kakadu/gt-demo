open Utils
open Printf

module PV : sig
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv :
  object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end
  class ['a, 'b] show_pv :
    (unit -> 'a -> string) ->
    (unit -> 'b -> string) ->
    (unit -> [> ('a,'b) pv] -> string) ->
    object
      method c_A : unit -> 'a -> string
      method c_B : unit -> 'b -> string
    end
  class ['a, 'a1, 'b, 'b1, 'self] gmap_pv :
    (unit -> 'a -> 'a1) ->
    (unit -> 'b -> 'b1) ->
    (unit -> [> ('a,'b) pv ] -> 'self) ->
    object
      constraint 'self = [> ('a1, 'b1) pv ]
      method c_A : unit -> 'a -> 'self
      method c_B : unit -> 'b -> 'self
    end

  val gcata_pv :
    (_,'a,'syn, _, 'b,'syn, 'inh, _, 'syn) #class_pv ->
    'inh -> ('a,'b) pv -> 'syn
  val show_pv :
    ('a -> string) -> ('b -> string) -> ('a,'b) pv -> string

  (* We add more subtyping polymorphism to the result type to avoid exlicit casts
   * when using the result of gmap_pv as an argument of show_pv_ext *)
  val gmap_pv : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) pv -> [> ('b,'d) pv ]
end = struct
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
  type ('self,'a,'b) pv_open = 'self
    constraint 'self = [> `A of 'a  | `B of 'b ]

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv =
  object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end

class ['a, 'b] show_pv fa fb self =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, _, string] class_pv
    method c_A () a = sprintf "`A (%a)" fa a
    method c_B () b = sprintf "`B (%a)" fb b
  end

(* I added an extra argument for the 'self variable in the open type
 * We need this in gmap class and not in a show implementation
*)
class ['a, 'a1, 'b, 'b1, 'extra ] gmap_pv
    (fa : unit -> 'a -> 'a1) (fb : unit -> 'b -> 'b1) self =
  object
    inherit [ unit, 'a, 'a1
            , unit, 'b, 'b1
            , unit, [> ('a1,'b1) pv ] as 'extra, 'extra] class_pv
    method c_A () a = match `A (fa () a) with #pv as x -> x
    method c_B () b = `B (fb () b)
  end

let rec gcata_pv tr inh p =
  match p with
  | `A a -> tr#c_A inh a
  | `B b -> tr#c_B inh b

let pv = {gcata = gcata_pv; plugins = object end}
let show_pv fa fb s = transform1(pv) (new show_pv (fun () -> fa) (fun () -> fb)) () s

let gmap_pv fa fb s = transform1(pv) (new gmap_pv (fun () -> fa) (fun () -> fb)) () s

end

let _ =
  let open PV in
  Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (show_pv id            id (`A "1"))
      (show_pv string_of_int id @@
       gmap_pv int_of_string id (`A "1"))


(* ********** 2nd declaration **************************************************** *)
module PVExt : sig
  open PV
  type ('a, 'b) pv_ext = [ ('a,'b) pv | `C of 'a ]
  (* type ('self,'a,'b) pv_ext_open = 'self
   *   constraint 'self = [> `C of 'a  | ('a, 'b) pv ] *)

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] class_pv_ext :
    object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] class_pv
      method virtual c_C : 'inh -> 'a -> 'syn
  end
  class ['a, 'b] show_pv_ext :
    (unit -> 'a -> string) ->
    (unit -> 'b -> string) ->
    (unit -> [> ('a,'b) pv_ext ] -> string) ->
    object
      inherit ['a,'b] show_pv
      method c_C : unit -> 'a -> string
    end
  class ['a, 'a2, 'b, 'b2, 'extra] gmap_pv_ext :
    (unit -> 'a -> 'a2) ->
    (unit -> 'b -> 'b2) ->
    (unit -> [> ('a,'b) pv_ext ] -> 'extra) ->
    object
      inherit ['a, 'a2, 'b, 'b2, 'extra ] gmap_pv
      constraint 'extra = [> ('a2, 'b2) pv_ext ]
      method c_C : unit -> 'a -> 'extra
    end

  val gcata_pv_ext :
    < (_,'a,'syn, _, 'b,'syn, 'inh, _, 'syn) class_pv
    ; c_C : 'inh -> 'a -> 'syn; .. > ->
    'inh -> [< `C of 'a | ('a,'b) pv ] -> 'syn

val show_pv_ext : ('a -> string) -> ('b -> string) ->
  (* [> ('a,'b) pv | `C of 'a ] -> *)
  ('a,'b) pv_ext ->
  (* [> ('a,'b) pv_ext ] ->   (\* seems to be wrong *\) *)
  string

  val gmap_pv_ext :
    ('a -> 'a2) -> ('b -> 'b2) -> ('a,'b) pv_ext -> [> ('a2, 'b2) pv_ext ]
end = struct
  open PV
  type ('a, 'b) pv_ext = [ `C of 'a | ('a, 'b) pv ]
  type ('self,'a,'b) pv_ext_open = 'self
    constraint 'self = [> `C of 'a  | ('a, 'b) pv ]

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] class_pv_ext =
    object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] class_pv
      method virtual c_C : 'inh -> 'a -> 'syn
    end

  class ['a, 'b] show_pv_ext (fa: unit -> 'a -> string) fb
      (self: unit -> [> ('a,'b) pv_ext ] -> string) =
    object
      inherit [ unit, 'a, string
              , unit, 'b, string
              , unit, ('a, 'b) pv_ext , string
              ] class_pv_ext
      inherit ['a, 'b] show_pv fa fb self
      method c_C () a = sprintf "`C (%a)" fa a
    end

  class ['a, 'a2, 'b, 'b2, 'extra] gmap_pv_ext
      (fa : unit -> 'a -> 'a2)
      (fb : unit -> 'b -> 'b2)
      (self: unit -> [> ('a,'b) pv_ext ] -> ('extra, 'a2,'b2) pv_ext_open) =
    object
      inherit [ unit, 'a, 'a2
              , unit, 'b, 'b2
              , unit, 'extra, ('extra, 'a2,'b2) pv_ext_open
              ] class_pv_ext
      inherit ['a, 'a2, 'b, 'b2, 'extra ] gmap_pv fa fb self
      method c_C () a : ('extra, 'a2,'b2) pv_ext_open =
        match `C (fa () a) with #pv_ext as x -> x
        (* `C (fa () a) *)
    end


let rec gcata_pv_ext tr inh p =
  match p with
  | `C a -> tr#c_C inh a
  | #pv as subj -> gcata_pv tr inh subj

let pv_ext = { gcata = gcata_pv_ext; plugins = object end}
let show_pv_ext fa fb s =
  transform1(pv_ext) (new show_pv_ext (fun () -> fa) (fun () -> fb)) () s

let gmap_pv_ext fa fb s =
  transform1(pv_ext) (new gmap_pv_ext (fun () -> fa) (fun () -> fb)) () s

end

let _ =
  let open PV in
  let open PVExt in
  Printf.printf "****************************\n%!";
  Printf.printf "Original pv: %s\n" @@
      show_pv  id            id (`A "1");
  Printf.printf "Mapped pv and showed as a pv_ext: %s\n" @@
    show_pv_ext string_of_int id
      (gmap_pv int_of_string id (`A "1") (* :> (int,string) pv_ext *));
  Printf.printf "Original pv_ext: %s\n" @@
    show_pv_ext id            id (`C "1");
  Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n" @@
    show_pv_ext string_of_int  id
      (gmap_pv_ext int_of_string id (`C "1"));


(* ******************************************************************************* *)

module PVExt2 : sig
  open PVExt
  type ('a, 'b) pv_ext2 = [ ('a,'b) pv_ext | `D of 'a ]
  (* type ('self,'a,'b) pv_ext2_open = 'self
   *   constraint 'self = [> `D of 'a  | ('a, 'b) pv_ext ] *)

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] class_pv_ext2 :
    object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] class_pv_ext
      method virtual c_D : 'inh -> 'a -> 'syn
    end
  class ['a, 'b] show_pv_ext2 :
    (unit -> 'a -> string) ->
    (unit -> 'b -> string) ->
    (unit -> [> ('a,'b) pv_ext2 ] -> string) ->
    object
      inherit ['a,'b] show_pv_ext
      method c_D : unit -> 'a -> string
    end

  (* autogenerated signature *)
  class ['a, 'a2, 'b, 'b2, 'c] gmap_pv_ext2 :
    (unit -> 'a -> 'a2) ->
    (unit -> 'b -> 'b2) ->
    (unit -> [> ('a, 'b) pv_ext2 ] -> 'c) ->
    object
      constraint 'c = [> ('a2, 'b2) pv_ext2 ]
      inherit ['a, 'a2, 'b, 'b2, 'c] gmap_pv_ext
      method c_D : unit -> 'a -> 'c
    end

  val gcata_pv_ext2 :
    <  (_,'a,'syn, _,'b,'syn, 'inh, _, 'syn) class_pv_ext
    ; c_D : 'inh -> 'a -> 'syn; .. > ->
    'inh -> ('a,'b) pv_ext2 -> 'syn
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
  (* type ('self,'a,'b) pv_ext2_open = 'self
   *   constraint 'self = [> ('a, 'b) pv_ext | `D of 'a ] *)

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] class_pv_ext2 =
  object
    inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'extra, 'syn] class_pv_ext
    method virtual c_D : 'inh -> 'a -> 'syn
  end

  class ['a, 'b] show_pv_ext2  (fa: unit -> 'a -> string) fb
      (self: unit -> [> ('a,'b) pv_ext2 ] -> string) =
    object
      inherit [ unit, 'a, string
              , unit, 'b, string
              , unit, ('a, 'b) pv_ext2, string
              ] class_pv_ext
      inherit ['a, 'b] show_pv_ext fa fb self
      method c_D () a = sprintf "`D (%a)" fa a
    end

  class ['a, 'a2, 'b, 'b2, 'extra] gmap_pv_ext2
    (fa : unit -> 'a -> 'a2)
    (fb : unit -> 'b -> 'b2)
    (self: unit -> [> ('a,'b) pv_ext2 ] -> ([> ('a2,'b2) pv_ext2 ] as 'extra) ) =
  object
    inherit [ unit, 'a, 'a2
            , unit, 'b, 'b2
            , unit, 'extra, [> ('a2,'b2) pv_ext2 ] as 'extra
            ] class_pv_ext2
    inherit ['a, 'a2, 'b, 'b2, 'extra ] gmap_pv_ext fa fb self
    method c_D () a : 'extra =
      match `D (fa () a) with #pv_ext2 as x -> x
  end

let rec gcata_pv_ext2 tr inh p =
  match p with
  | `D a -> tr#c_D inh a
  | #pv_ext as subj -> gcata_pv_ext tr inh subj

let pv_ext2 = { gcata = gcata_pv_ext2; plugins = object end }
let show_pv_ext2 fa fb s =
  transform1(pv_ext2) (new show_pv_ext2 (fun () -> fa) (fun () -> fb)) () s

let gmap_pv_ext2 fa fb s =
  transform1(pv_ext2) (new gmap_pv_ext2 (fun () -> fa) (fun () -> fb)) () s

end

let _ =
  let open PVExt in
  let open PVExt2 in

  Printf.printf "****************************\n%!";
  Printf.printf "Original pv_ext: %s\n" @@
      show_pv_ext  id            id (`A "1");
  Printf.printf "Mapped pv_ext and showed as a pv_ext2: %s\n" @@
    show_pv_ext2 string_of_int id
      (gmap_pv_ext int_of_string id (`C "1") );
  Printf.printf "Original pv_ext2: %s\n" @@
    show_pv_ext2 id            id (`D "1");
  Printf.printf "Mapped PV_ext2 and showed as a pv_ext2: %s\n" @@
    show_pv_ext2 string_of_int  id
      (gmap_pv_ext2 int_of_string id (`D "1"));
