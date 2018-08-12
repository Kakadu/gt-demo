(* mutal recursion *)
open Utils
open Printf

let __ = Obj.magic ()

type 'l a = A of b     | C | E of 'l a | D of 'l
and     b = I of int a | J | K of b
and     c = string a

let rec gcata_a tr inh = function
  | A a -> tr#c_A inh a
  | C   -> tr#c_C inh
  | E a -> tr#c_E inh a
  | D l -> tr#c_D inh l
let rec gcata_b tr inh = function
  | I b -> tr#c_I inh b
  | J   -> tr#c_J inh
  | K b -> tr#c_K inh b

class virtual [ 'il, 'l, 'sl, 'inh, 'self, 'syn ] class_a = object
  method virtual c_A   : 'inh -> b    -> 'syn
  method virtual c_C   : 'inh         -> 'syn
  method virtual c_E   : 'inh -> 'l a -> 'syn
  method virtual c_D   : 'inh ->   'l -> 'syn
end
class virtual [ 'inh, 'self, 'syn ] class_b = object
  method virtual c_I   : 'inh -> int a -> 'syn
  method virtual c_J   : 'inh          -> 'syn
  method virtual c_K   : 'inh ->     b -> 'syn
end



class ['l, 'self] show_a_stub ob fself fl = object
  inherit [unit, 'l, string, unit, 'self, string] class_a
  method c_A () be = sprintf "A (%a)" (gcata_b @@ ob ()) be
  method c_C ()    = "C"
  method c_E () a  = sprintf "E (%a)" fself a
  method c_D () l  = sprintf "D (%a)" fl l
end
class ['self] show_b_stub oa fself = object
  inherit [unit, 'self, string] class_b
  method c_I () a  =
    (* if type `a` is not used in b than fl doesn't matter
       if it is used we know the type of argument
       if it is used with two different types than it is regularity restriction
    *)
    let fl () n = string_of_int n in
    sprintf "I (%a)" (gcata_a @@ oa () fl) a
  method c_J ()    = "J"
  method c_K () b  = sprintf "K (%a)" fself b
end

type typ_for_a = { a_func :
                     'a 'selfa 'selfb . (unit -> (unit, 'selfb, string) class_b) ->
                     (unit -> 'a a -> string) ->
                     (unit -> 'a -> string) ->
                     ('a, 'selfa) show_a_stub
                 }
type typ_for_b = { b_func :
                     'a 'selfa 'selfb .
                       (unit -> (unit -> int -> string) ->
                                (unit, int, string, unit, 'selfa, string) class_a) ->
                       (unit -> b -> string) ->
                       'selfb show_b_stub
                 }
let (a0,b0) = ({ a_func = new show_a_stub}, {b_func = new show_b_stub})
type a_trf  = { a_trf: 'a . (unit -> 'a -> string) -> unit -> 'a a -> string }
type b_trf  = { b_trf:      unit -> b -> string }
type oa_func= { oa_func: 'a 'selfa .
                         unit ->
                         (unit -> 'a -> string) ->
                         (unit, 'a, string, unit, 'selfa, string) class_a }
type ob_func= { ob_func: 'selfb . unit -> (unit, 'selfb, string) class_b }


let myfix (a0,b0) =
  let rec show_a = { a_trf = fun fl inh a -> gcata_a (oa.oa_func () fl) inh a }
  and     oa     = { oa_func = fun () fl -> a0.a_func ob.ob_func (show_a.a_trf fl) fl }
  and     ob     = { ob_func = fun ()    -> b0.b_func oa.oa_func show_b.b_trf }
  and     show_b = { b_trf = (fun inh b -> gcata_b (ob.ob_func ()) inh b) }
  in
  (show_a, show_b)

let (fix_result_1, fix_result_2) = myfix (a0,b0)

let show_a fa a = fix_result_1.a_trf (fun () -> fa) () a
let show_b    b = fix_result_2.b_trf                () b

let _ =
  let show_int = string_of_int in
  printf "Stage 1\n";
  printf "%s\n" @@ show_a show_int (E C);
  printf "%s\n" @@ show_a show_int (A (I C));
  printf "%s\n" @@ show_b          (I (A J));
  printf "%s\n" @@ show_b          (K J);
  ()


(* Reimplementing some stuff *)


class ['l, 'self] show_a_stub2 ob fself fl = object
  inherit ['l, 'self] show_a_stub ob fself fl
  method! c_A () be = sprintf "A %a" (gcata_b @@ ob ()) be
end

let (a0,b0) = ({ a_func = new show_a_stub2}, {b_func = new show_b_stub})

let (fix_result_1, fix_result_2) = myfix (a0,b0)

let show_a fa a = fix_result_1.a_trf fa a
let show_b    b = fix_result_2.b_trf b

let _ =
  let show_int    () n = string_of_int n in
  printf "%s\n" @@ show_a show_int () (E C);
  printf "%s\n" @@ show_a show_int () (A (I C));
  printf "%s\n" @@ show_b () (I (A J));
  printf "%s\n" @@ show_b () (K J);
  ()

(** gmap *********************************************** *)
class ['l, 'l2, 'self] gmap_a_stub ob fself fl = object
  inherit [unit, 'l, 'l2, unit, 'self, 'l2 a] class_a
  method c_A () be = A (gcata_b (ob ()) () be)
  method c_C ()    = C
  method c_E () a  = E (fself () a)
  method c_D () l  =
    print_endline "gmapping D";
    D (fl () l)
end
class ['self] gmap_b_stub oa fself = object
  inherit [unit, 'self, b] class_b
  method c_I () a  =
    (* if type `a` is not used in b than fl doesn't matter
       if it is uses we know the type of argument
       if it is used with two different types than it is regularity restriction
    *)
    let fl () _ =  assert false in
    I (gcata_a (oa () fl) () a)
  method c_J ()    = J
  method c_K () b  = K (fself () b)
end

type gmap_typ_for_a =
  { gmap_a_func :
      'a 'l2 'selfa 'selfb .
        (unit -> (unit, 'selfb, b) class_b) ->
        (unit -> 'a a -> 'l2 a) ->
        (unit -> 'a -> 'l2) ->
        ('a, 'l2, 'selfa) gmap_a_stub
  }
type gmap_typ_for_b =
  { gmap_b_func :
      'a 'a2 'selfa 'selfb .
        (unit -> (unit -> 'a -> 'a2) ->
         (unit, int, string, unit, 'selfa, int a) class_a) ->
      (unit -> b -> b) ->
      'selfb gmap_b_stub
  }

let (a0,b0) = ({ gmap_a_func = new gmap_a_stub}, {gmap_b_func = new gmap_b_stub})

type gmap_a_2  = { gmap_a_trf: 'a 'b . (unit -> 'a -> 'b) -> unit -> 'a a -> 'b a }
type gmap_b_2  = { gmap_b_trf:          unit -> b -> b }
type gmap_oa_3 = { gmap_oa_func:
                  'a 'b 'selfa .
                    unit ->
                  (unit -> 'a -> 'b) ->
                  (unit, 'a, 'b, unit, 'selfa, 'b a) class_a
              }
type gmap_ob_3 = { gmap_ob_func: 'selfb . unit -> (unit, 'selfb, b) class_b }


let (gmap_a0,gmap_b0) =
  ({ gmap_a_func = new gmap_a_stub}, {gmap_b_func = new gmap_b_stub})

let (fix_result_1, fix_result_2) =
  let gmap_myfix (a0,b0) =
    let rec p_a = { gmap_a_trf = fun fl inh a ->
        (* Eta-expansion matters !!! *)
        gcata_a (oa.gmap_oa_func () fl) inh a }
    and     p_b = { gmap_b_trf = fun inh b -> gcata_b (ob.gmap_ob_func ()) inh b }
    and     oa  = { gmap_oa_func = fun () fl ->
                      a0.gmap_a_func ob.gmap_ob_func (p_a.gmap_a_trf fl) fl }
    and     ob  = { gmap_ob_func = fun () ->
                      b0.gmap_b_func oa.gmap_oa_func (p_b.gmap_b_trf) }
    in
    (p_a, p_b)
  in
  gmap_myfix (gmap_a0,gmap_b0)

(* extra hacks to skip inherited attribute *)
let gmap_a fa a = fix_result_1.gmap_a_trf  (fun () -> fa) () a
let gmap_b    b = fix_result_2.gmap_b_trf                 () b

let _ =
  let show_int    () n = string_of_int n in
  let t1 = D 5 in
  printf "Stage 2\n";
  printf "%s\n" @@ show_a show_int () @@ t1;
  printf "%s\n" @@ show_a show_int () @@ gmap_a ((+)1) t1;
  ()


(* ***************************** Fmt now ******************************* *)
class ['l, 'self] fmt_a_stub ob fself fl = object
  inherit [Format.formatter, 'l, unit, Format.formatter, 'self, unit] class_a
  method c_A fmt be = Format.fprintf fmt "A (%a)" (gcata_b (ob ())) be
  method c_C fmt    = Format.fprintf fmt "C"
  method c_E fmt a  = Format.fprintf fmt "E (%a)" fself a
  method c_D fmt l  = Format.fprintf fmt "D (%a)" fl l
end
class ['self] fmt_b_stub oa fself = object
  inherit [Format.formatter, 'self, unit] class_b
  method c_I fmt a  =
    (* if type `a` is not used in b than fl doesn't matter
       if it is uses we know the type of argument
       if it is used with two different types than it is regularity restriction
    *)
    let fl fmt = Format.fprintf fmt "%d" in
    Format.fprintf fmt "I (%a)"  (gcata_a (oa () fl)) a
  method c_J fmt    = Format.fprintf fmt "J"
  method c_K fmt b  = Format.fprintf fmt "K (%a)" fself b
end

type fmt_typ_for_a =
  { fmt_a_func :
      'a 'selfa 'selfb .
        (unit -> (Format.formatter, 'selfb, unit) class_b) ->
        (Format.formatter -> 'a a -> unit) ->
        (Format.formatter -> 'a -> unit) ->
        ('a, 'selfa) fmt_a_stub
  }
type fmt_typ_for_b =
  { fmt_b_func :
      'selfa 'selfb .
        (unit -> (Format.formatter -> int -> unit) ->
         (Format.formatter, int, unit, Format.formatter, 'selfa, unit) class_a) ->
      (Format.formatter -> b -> unit) ->
      'selfb fmt_b_stub
  }

type fmt_a_2  = { fmt_a_trf: 'a 'b . (Format.formatter -> 'a -> unit) ->
                    Format.formatter -> 'a a -> unit
                }
type fmt_b_2  = { fmt_b_trf:          Format.formatter -> b -> unit }
type fmt_oa_3 =
  { fmt_oa_func:
      'a  'selfa .
        unit ->
      (Format.formatter -> 'a -> unit) ->
      (Format.formatter, 'a, unit, Format.formatter, 'selfa, unit) class_a
  }
type fmt_ob_3 =
  { fmt_ob_func: 'selfb . unit -> (Format.formatter, 'selfb, unit) class_b }


let (fmt_a0,fmt_b0) = ({ fmt_a_func = new fmt_a_stub}, {fmt_b_func = new fmt_b_stub})

let (fix_result_1, fix_result_2) =
  let fmt_myfix (a0,b0) =
    let rec p_a = { fmt_a_trf = fun fl inh a -> gcata_a (oa.fmt_oa_func () fl) inh a}
    and     p_b = { fmt_b_trf = fun inh b -> gcata_b (ob.fmt_ob_func ()) inh b }
    and     oa  = { fmt_oa_func = fun () fl ->
                      a0.fmt_a_func ob.fmt_ob_func (p_a.fmt_a_trf fl) fl }
    and     ob  = { fmt_ob_func = fun () ->
                      b0.fmt_b_func oa.fmt_oa_func (p_b.fmt_b_trf) }
    in
    (p_a, p_b)
  in
  fmt_myfix (fmt_a0,fmt_b0)

(* extra hacks to skip inherited attribute *)
let fmt_a fa fmt a = fix_result_1.fmt_a_trf fa fmt  a
let fmt_b    fmt b = fix_result_2.fmt_b_trf    fmt  b

let _ =
  let fmt_int fmt n = Format.fprintf fmt "%d" n in
  let t1 = D 5 in
  Format.printf "Stage 3\n";
  Format.printf "%a\n" (fmt_a fmt_int) t1;
  Format.printf "%a\n" (fmt_a fmt_int) (A (I C));
  Format.printf "%a\n" fmt_b (I (A J));
  Format.printf "%a\n" fmt_b (K J);
  ()
