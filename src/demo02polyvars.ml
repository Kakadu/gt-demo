open Utils
open Printf

module PV : sig
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv :
  object
    method virtual c_A : 'inh -> ('a,'b) pv -> 'a -> 'syn
    method virtual c_B : 'inh -> ('a,'b) pv -> 'b -> 'syn
  end
  class ['a, 'b] show_pv_t :
    ('a -> string) ->
    ('b -> string) ->
    ( ('a,'b) pv -> string) ->
    object
      inherit [ unit, 'a, string, unit, 'b, string, unit, 'self, string] class_pv
      method c_A : unit -> ('a,'b) pv -> 'a -> string
      method c_B : unit -> ('a,'b) pv -> 'b -> string
    end
  class ['a, 'a1, 'b, 'b1, 'self] gmap_pv_t :
    ('a -> 'a1) ->
    ('b -> 'b1) ->
    (('a,'b) pv -> 'self) ->
    object
      inherit [ unit, 'a, 'a1
              , unit, 'b, 'b1
              , unit, 'self,  'self] class_pv
      constraint 'self = [> ('a1,'b1) pv ]
      method c_A : unit -> ('a,'b) pv -> 'a -> 'self
      method c_B : unit -> ('a,'b) pv -> 'b -> 'self
    end

  val gcata_pv :
    (_,'a,'syn, _,'b,'syn, 'inh, _, 'syn) #class_pv ->
    'inh -> ('a,'b) pv -> 'syn

  val show_pv :
    ('a -> string) -> ('b -> string) -> ('a,'b) pv -> string

  (* We add more subtyping polymorphism to the result type to avoid exlicit casts
   * when using the result of gmap_pv as an argument of show_pv_ext *)
  val gmap_pv : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) pv -> [> ('b,'d) pv ]
end = struct
  type ('a, 'b) pv = [ `A of 'a | `B of 'b ]

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv =
  object
    method virtual c_A : 'inh -> ('a,'b) pv -> 'a -> 'syn
    method virtual c_B : 'inh -> ('a,'b) pv -> 'b -> 'syn
  end

  let gcata_pv tr inh (s : (_,_) pv) =
    match s with
    | `A a -> tr#c_A inh s a
    | `B b -> tr#c_B inh s b

  class ['a, 'b] show_pv_t fa fb fself = object
    inherit [unit, 'a, string, unit, 'b, string, unit, 'self, string] class_pv
    method c_A () _ a = sprintf "`A (%s)" (fa a)
    method c_B () _ b = sprintf "`B (%s)" (fb b)
  end

  (* I added an self argument for the 'self variable in the open type
   * We need this in gmap class and not in a show implementation
  *)
  class ['a, 'a1, 'b, 'b1, 'self ] gmap_pv_t (fa : 'a -> 'a1) (fb : 'b -> 'b1) fself =
  object
    inherit [ unit, 'a, 'a1
            , unit, 'b, 'b1
            , unit, 'self, 'self] class_pv
    constraint 'self = [> ('a1,'b1) pv ]
    method c_A () _ a = match `A (fa a) with #pv as x -> x
    method c_B () _ b = `B (fb b)
  end

  let pv = { gcata = gcata_pv; }

  let show_pv fa fb t = transform(pv) (new show_pv_t fa fb) t
  let gmap_pv fa fb t = transform(pv) (new gmap_pv_t fa fb) t
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

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv_ext :
    object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv
      method virtual c_C : 'inh -> ('a, 'b) pv_ext -> 'a -> 'syn
  end
  class ['a, 'b] show_pv_ext : ('a -> string) -> ('b -> string) ->
    (('a,'b) pv_ext -> string) ->
    object
      inherit ['a,'b] show_pv_t
      method c_C : unit -> ('a, 'b) pv_ext -> 'a -> string
    end

  class ['a, 'a2, 'b, 'b2, 'self] gmap_pv_ext : ('a -> 'a2) -> ('b -> 'b2) ->
    ( ('a,'b) pv_ext -> 'self) ->
    object
      constraint 'self = [> ('a2, 'b2) pv_ext ]
      inherit ['a, 'a2, 'b, 'b2, 'self ] gmap_pv_t
      method c_C : unit -> ('a, 'b) pv_ext -> 'a -> 'self
    end

  val gcata_pv_ext :
    < (_,'a,'syn, _,'b,'syn, 'inh, _, 'syn) class_pv
    ; c_C : 'inh -> ('a, 'b) pv_ext -> 'a -> 'syn; .. > ->
    'inh -> ('a,'b) pv_ext -> 'syn

  val show_pv_ext : ('a -> string) -> ('b -> string) ->
    ('a,'b) pv_ext ->
    string

  val gmap_pv_ext :
    ('a -> 'a2) -> ('b -> 'b2) -> ('a,'b) pv_ext -> [> ('a2, 'b2) pv_ext ]
end = struct
  open PV
  type ('a, 'b) pv_ext = [ `C of 'a | ('a, 'b) pv ]

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv_ext =
    object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv
      method virtual c_C : 'inh -> ('a, 'b) pv_ext -> 'a -> 'syn
    end

  class ['a, 'b] show_pv_ext (fa: 'a -> string) fb (fself: ('a,'b) pv_ext -> string)
    =
    object
      inherit [ unit, 'a, string
              , unit, 'b, string
              , unit, ('a, 'b) pv_ext, string
              ] class_pv_ext
      inherit ['a, 'b] show_pv_t fa fb (function #pv as subj -> fself subj)
      method c_C () _ a = sprintf "`C (%s)" (fa a)
    end

  class ['a, 'a2, 'b, 'b2, 'self] gmap_pv_ext
      (fa : 'a -> 'a2)
      (fb : 'b -> 'b2)
      (fself: ('a,'b) pv_ext -> 'self)
    =
    object
      inherit [ unit, 'a, 'a2
              , unit, 'b, 'b2
              , unit, 'self, 'self
              ] class_pv_ext
      constraint 'self = [> ('a2,'b2) pv_ext ]
      inherit ['a, 'a2, 'b, 'b2, 'self ] gmap_pv_t fa fb
          (function #pv as s -> fself s)
      method c_C () _ a =
        match `C (fa a) with #pv_ext as x -> x

    end

  let gcata_pv_ext tr inh s =
    match s with
    | #pv as subj -> gcata_pv tr inh subj
    | `C a        -> tr#c_C inh s a

  let pv_ext = { gcata = gcata_pv_ext }

  let show_pv_ext fa fb s =
    transform0(pv_ext) (new show_pv_ext fa fb) s

  let gmap_pv_ext fa fb s =
    transform0(pv_ext) (new gmap_pv_ext fa fb) s

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

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv_ext2 :
    object
      inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv_ext
      method virtual c_D : 'inh -> ('a, 'b) pv_ext2 -> 'a -> 'syn
    end
  class ['a, 'b] show_pv_ext2 :
    ('a -> string) ->
    ('b -> string) ->
    (('a,'b) pv_ext2 -> string) ->
    object
      inherit ['a,'b] show_pv_ext
      method c_D : unit -> ('a, 'b) pv_ext2 -> 'a -> string
    end

  (* autogenerated signature *)
  class ['a, 'a2, 'b, 'b2, 'extra] gmap_pv_ext2 :
    ('a -> 'a2) ->
    ('b -> 'b2) ->
    (('a, 'b) pv_ext2 -> 'extra) ->
    object
      constraint 'extra = [> ('a2, 'b2) pv_ext2 ]
      inherit ['a, 'a2, 'b, 'b2, 'extra] gmap_pv_ext
      method c_D : unit -> ('a, 'b) pv_ext2 -> 'a -> 'extra
    end

  (* These doesn't *)
  (* class ['a, 'a2, 'b, 'b2, 'self] map_pv_ext2 :
   *   (unit -> [> ('a,'b) pv_ext2 ] -> [> ('a2,'b2) pv_ext2]) ->
   *   (unit -> 'a -> 'a2) ->
   *   (unit -> 'b -> 'b2) ->
   *   object
   *     constraint 'self = [> ('a2,'b2) pv_ext2 ]
   *     inherit ['a, 'a2, 'b, 'b2, 'self ] map_pv_ext
   *     method c_D : unit -> 'a -> ([> ('a2,'b2) pv_ext2 ])
   *   end *)

  (* next one works but have ext_open types *)
  (* class ['a, 'a2, 'b, 'b2, 'self] map_pv_ext2 :
   *   (unit -> [> ('a,'b) pv_ext2 ] -> ('self, 'a2,'b2) pv_ext2_open) ->
   *   (unit -> 'a -> 'a2) ->
   *   (unit -> 'b -> 'b2) ->
   *   object
   *     inherit ['a, 'a2, 'b, 'b2, 'self ] map_pv_ext
   *     method c_D : unit -> 'a -> ([> ('a2,'b2) pv_ext2 ] as 'self)
   *   end *)

  val gcata_pv_ext2 :
    < (_,'a,'syn, _,'b,'syn, 'inh, _, 'syn) class_pv_ext
    ; c_D : 'inh -> ('a, 'b) pv_ext2 -> 'a -> 'syn; .. > ->
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

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv_ext2 =
  object
    inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_pv_ext
    method virtual c_D : 'inh -> ('a, 'b) pv_ext2 -> 'a -> 'syn
  end

  class ['a, 'b] show_pv_ext2 (fa: 'a -> string) fb
      (fself: ('a,'b) pv_ext2 -> string) =
    object
      inherit [ unit, 'a, 'a2
              , unit, 'b, 'b2
              , unit, [> ('a2,'b2) pv_ext2 ], 'self
              ] class_pv_ext2
      inherit ['a, 'b] show_pv_ext fa fb (function #pv_ext as s -> fself s)
      method c_D () _ a = sprintf "`D (%s)" (fa a)
    end

  class ['a, 'a2, 'b, 'b2, 'self] gmap_pv_ext2
      (fa : 'a -> 'a2)
      (fb : 'b -> 'b2)
      (fself: ('a,'b) pv_ext2 -> 'self) =
  object
    inherit [ unit, 'a, 'a2
            , unit, 'b, 'b2
            , unit, 'self, 'self
            ] class_pv_ext2
    constraint 'self = [> ('a2,'b2) pv_ext2 ]
    inherit ['a, 'a2, 'b, 'b2, 'self ] gmap_pv_ext fa fb
        (function #pv_ext as s -> fself s)
    method c_D () _ a =
      match `D (fa a) with #pv_ext2 as x -> x
  end

  let gcata_pv_ext2 tr inh s =
    match s with
    | `D a -> tr#c_D inh s a
    | #pv_ext as subj -> gcata_pv_ext tr inh subj

  let pv_ext2 = { gcata = gcata_pv_ext2 }
  let show_pv_ext2 fa fb s =
    transform0(pv_ext2) (new show_pv_ext2 fa fb) s

  let gmap_pv_ext2 fa fb s =
    transform0(pv_ext2) (new gmap_pv_ext2 fa fb) s

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



(* ********************************** Completely another type *********************** *)
module PV2 : sig
  type 'a pv2 = [ `F of 'a ]

  class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] class_pv2_t : object
    method virtual c_F : 'inh -> 'a pv2 -> 'a -> 'syn
  end
  class ['a] show_pv2_t :
    ('a -> string) ->
    ( 'a pv2 -> string) ->
    object
      inherit [ unit, 'a, string, unit, 'self, string] class_pv2_t
      method c_F : unit -> 'a pv2 -> 'a -> string
    end
  class ['a, 'a1, 'self] gmap_pv2_t :
    ('a -> 'a1) ->
    ('a pv2 -> 'self) ->
    object
      constraint 'self = [> 'a1 pv2 ]
      inherit [ unit, 'a, 'a1
              , unit, 'self, 'self] class_pv2_t
      method c_F : unit -> 'a pv2 -> 'a -> 'self
    end

  val gcata_pv2 :
    (_,'a,'syn, 'inh, _, 'syn) #class_pv2_t ->
    'inh -> 'a pv2 -> 'syn

  val show_pv2 :
    ('a -> string) -> 'a pv2 -> string

  (* We add more subtyping polymorphism to the result type to avoid exlicit casts
   * when using the result of gmap_pv as an argument of show_pv_ext *)
  val gmap_pv2 : ('a -> 'b) -> 'a pv2 -> [> 'b pv2 ]
end = struct
  type 'a pv2 = [ `F of 'a ]

  class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] class_pv2_t =
  object
    method virtual c_F : 'inh -> 'a pv2 -> 'a -> 'syn
  end

  let gcata_pv2 tr inh (s : _ pv2) = match s with
    | `F a -> tr#c_F inh s a

  class ['a] show_pv2_t fa fself = object
    inherit [unit, 'a, string, unit, 'self, string] class_pv2_t
    method c_F () _ a = sprintf "`F (%s)" (fa a)
  end

  (* I added an self argument for the 'self variable in the open type
   * We need this in gmap class and not in a show implementation
  *)
  class ['a, 'a1, 'self ] gmap_pv2_t (fa : 'a -> 'a1) fself =
  object
    inherit [ unit, 'a, 'a1
            , unit, 'self, [> 'a1 pv2 ] as 'self] class_pv2_t
    method c_F () _ a = match `F (fa a) with #pv2 as x -> x
  end

  let pv2 = { gcata = gcata_pv2 }

  let show_pv2 fa t = transform(pv2) (new show_pv2_t fa) t
  let gmap_pv2 fa t = transform(pv2) (new gmap_pv2_t fa) t
end

let _ =
  let open PV2 in
  Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (show_pv2 id            (`F "1"))
      (show_pv2 string_of_int @@
       gmap_pv2 int_of_string  (`F "1"))


(* ********************************** Union of PV & PV2  *********************** *)

module PVSum = struct
  type ('a,'b) s = [ ('a,'b) PV.pv | 'a PV2.pv2 ]

  class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_s_t =
  object
    inherit ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn]  PV.class_pv
    inherit ['ia, 'a, 'sa              , 'inh, 'self, 'syn] PV2.class_pv2_t
  end

  class ['a, 'b] show_s_t fa fb fself =
    object
      inherit [ unit, 'a, string, unit, 'b, string, unit, 'self, string] class_s_t
      inherit ['a, 'b]  PV.show_pv_t fa fb (function  #PV.pv  as s -> fself s)
      inherit ['a]      PV2.show_pv2_t  fa (function #PV2.pv2 as s -> fself s)
    end

  class ['a, 'a1, 'b, 'b1, 'self ] gmap_s_t fa fb  fself =
  object
    inherit [ unit, 'a, 'a1
            , unit, 'b, 'b1
            , unit, 'self, [> ('a1,'b1) s ] as 'self] class_s_t
    inherit ['a, 'a1, 'b, 'b1, 'self ]  PV.gmap_pv_t  fa fb (function #PV.pv as s -> fself s)
    inherit ['a, 'a1,          'self ] PV2.gmap_pv2_t fa (function #PV2.pv2 as s -> fself s)
  end

  let gcata_s tr inh s =
    match s with
    |  #PV.pv  as subj -> PV.gcata_pv tr inh subj
    | #PV2.pv2 as subj -> PV2.gcata_pv2 tr inh subj

  let s = { gcata = gcata_s }
  let show_s fa fb subj =
    transform0(s) (new show_s_t fa fb) subj

  let gmap_s fa fb subj =
    transform0(s) (new gmap_s_t fa fb) subj

end

let _ =
  let open PVSum in

  let f x =
    Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (show_s id            id x)
      (show_s string_of_int id @@
       gmap_s int_of_string id x)
  in

  f (`F "1");
  f (`A "2");
  f (`B "3");
