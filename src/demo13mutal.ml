(* Demo 12: mutal recursion with less objects *)
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

class virtual [ 'il, 'l, 'sl, 'inh, 'self, 'syn ] a_t = object
  method virtual c_A   : 'inh -> b    -> 'syn
  method virtual c_C   : 'inh         -> 'syn
  method virtual c_E   : 'inh -> 'l a -> 'syn
  method virtual c_D   : 'inh ->   'l -> 'syn
end
class virtual [ 'inh, 'self, 'syn ] b_t = object
  method virtual c_I   : 'inh -> int a -> 'syn
  method virtual c_J   : 'inh          -> 'syn
  method virtual c_K   : 'inh ->     b -> 'syn
end

(* Final transformation function here *)
type a_trf  = { a_trf: 'a . ('a -> string) -> 'a a -> string }
type b_trf  = { b_trf:      b -> string }

class ['l, 'self] show_a_stub ob ~fself fl = object
  inherit [unit, 'l, string, unit, 'self, string] a_t
  method c_A () be = sprintf "A (%s)" (ob.b_trf be)
  method c_C ()    = "C"
  method c_E () a  = sprintf "E (%a)" (fun () -> fself.a_trf fl) a
  method c_D () l  = sprintf "D (%a)" (fun () -> fl) l
end
class ['self] show_b_stub oa ~fself = object
  inherit [unit, 'self, string] b_t
  method c_I () a  =
    (* if type `a` is not used in b than fl doesn't matter
       if it is used we know the type of argument
       if it is used with two different types than it is regularity restriction
    *)
    let fl n = string_of_int n in
    sprintf "I (%s)" (oa.a_trf fl a)
  method c_J ()    = "J"
  method c_K () b  = sprintf "K (%a)" (fun () -> fself.b_trf) b
end

(* Type of first arguments of generated classes *)
(* type oa_func= { oa_func: 'a 'selfa .
 *                          unit ->
 *                          ('a -> string) ->
 *                          ('a, 'selfa) show_a_stub }
 * type ob_func= { ob_func: 'selfb . unit -> 'selfb show_b_stub } *)


type typ_for_a =
  { a_func : 'a 'selfa .
               b_trf ->
      fself: a_trf ->
      ('a -> string) ->
      ('a, 'selfa) show_a_stub
  }
type typ_for_b =
  { b_func :  'a 'selfb .
                a_trf ->
      fself: b_trf ->
      'selfb show_b_stub
  }

let (a0,b0) = ({ a_func = new show_a_stub}, {b_func = new show_b_stub})

let myfix (a0,b0) =
  let rec show_a = { a_trf = fun fl a -> gcata_a (a0.a_func show_b ~fself:show_a fl) () a }
  and     show_b = { b_trf = fun    b -> gcata_b (b0.b_func show_a ~fself:show_b   ) () b }
  in
  (show_a, show_b)

let (fix_result_1, fix_result_2) = myfix (a0,b0)

let show_a fa a = fix_result_1.a_trf fa a
let show_b    b = fix_result_2.b_trf    b

let _ =
  let show_int = string_of_int in
  printf "Stage 1\n";
  printf "%s\n" @@ show_a show_int (E C);
  printf "%s\n" @@ show_a show_int (A (I C));
  printf "%s\n" @@ show_b          (I (A J));
  printf "%s\n" @@ show_b          (K J);
  ()


(* Reimplementing some stuff *)


(* class ['l, 'self] show_a_stub2 ob fself fl = object
 *   inherit ['l, 'self] show_a_stub ob fself fl
 *   method! c_A () be = sprintf "A %a" (gcata_b @@ ob ()) be
 * end
 *
 * let (a0,b0) = ({ a_func = new show_a_stub2}, {b_func = new show_b_stub})
 *
 * let (fix_result_1, fix_result_2, oa, ob) = myfix (a0,b0)
 *
 * class ['l, 'self] show_a fself fa = object
 *   inherit ['l, 'self] show_a_stub ob.ob_func fself fa
 * end
 * let show_a fa a = fix_result_1.a_trf fa a
 * let show_b    b = fix_result_2.b_trf b
 *
 * let _ =
 *   let show_int n = string_of_int n in
 *   printf "%s\n" @@ show_a show_int (E C);
 *   printf "%s\n" @@ show_a show_int (A (I C));
 *   printf "%s\n" @@ show_b (I (A J));
 *   printf "%s\n" @@ show_b (K J);
 *   () *)

(* ******************  Let's reuse type a as an alias ************ *)
(* type 'a ccc = 'a a
 * let gcata_ccc = gcata_a
 * class virtual ['ia,'a,'sa, 'inh, 'self, 'syn] ccc_t = object
 *   inherit ['ia,'a,'sa, 'inh, 'self, 'syn] a_t
 * end
 * class ['a, 'selfc] show_c fself fa = object
 *   inherit ['a, 'selfc] show_a fself fa
 * end
 * let show_c fa c =
 *   fix0 (fun fself -> gcata_ccc (new show_c fself fa) ()) c
 * let _ =
 *   let show_int n = string_of_int n in
 *   printf "Testing 'a c:\n%!";
 *   printf "%s\n" @@ show_c show_int (E C);
 *   printf "%s\n" @@ show_c show_int (A (I C));
 *   ()
 *
 *
 * (\** gmap *********************************************** *\)
 * class ['l, 'l2, 'self] gmap_a_stub ob fself fl = object
 *   inherit [unit, 'l, 'l2, unit, 'self, 'l2 a] a_t
 *   method c_A () be = A (gcata_b (ob ()) () be)
 *   method c_C ()    = C
 *   method c_E () a  = E (fself a)
 *   method c_D () l  = D (fl l)
 * end
 * class ['self] gmap_b_stub oa fself = object
 *   inherit [unit, 'self, b] b_t
 *   method c_I () a  =
 *     (\* if type `a` is not used in b than fl doesn't matter
 *        if it is uses we know the type of argument
 *        if it is used with two different types than it is regularity restriction
 *     *\)
 *     let fl _ =  assert false in
 *     I (gcata_a (oa () fl) () a)
 *   method c_J ()    = J
 *   method c_K () b  = K (fself b)
 * end
 *
 * type gmap_typ_for_a =
 *   { gmap_a_func :
 *       'a 'l2 'selfa 'selfb .
 *         (unit -> (unit, 'selfb, b) b_t) ->
 *         ('a a -> 'l2 a) ->
 *         ('a -> 'l2) ->
 *         ('a, 'l2, 'selfa) gmap_a_stub
 *   }
 * type gmap_typ_for_b =
 *   { gmap_b_func :
 *       'a 'a2 'selfa 'selfb .
 *         (unit -> ('a -> 'a2) ->
 *          (unit, int, string, unit, 'selfa, int a) a_t) ->
 *       (b -> b) ->
 *       'selfb gmap_b_stub
 *   }
 *
 * let (a0,b0) = ({ gmap_a_func = new gmap_a_stub}, {gmap_b_func = new gmap_b_stub})
 *
 * type gmap_a_2  = { gmap_a_trf: 'a 'b . ('a -> 'b) -> 'a a -> 'b a }
 * type gmap_b_2  = { gmap_b_trf:          b -> b }
 * type gmap_oa_3 = { gmap_oa_func:
 *                   'a 'b 'selfa .
 *                     unit ->
 *                   ('a -> 'b) ->
 *                   (unit, 'a, 'b, unit, 'selfa, 'b a) a_t
 *               }
 * type gmap_ob_3 = { gmap_ob_func: 'selfb . unit -> (unit, 'selfb, b) b_t }
 *
 *
 * let (gmap_a0,gmap_b0) =
 *   ({ gmap_a_func = new gmap_a_stub}, {gmap_b_func = new gmap_b_stub})
 *
 * let (fix_result_1, fix_result_2) =
 *   let gmap_myfix (a0,b0) =
 *     let rec p_a = { gmap_a_trf = fun fl a ->
 *         (\* Eta-expansion matters !!! *\)
 *         gcata_a (oa.gmap_oa_func () fl) () a }
 *     and     p_b = { gmap_b_trf = fun b -> gcata_b (ob.gmap_ob_func ()) () b }
 *     and     oa  = { gmap_oa_func = fun () fl ->
 *                       a0.gmap_a_func ob.gmap_ob_func (p_a.gmap_a_trf fl) fl }
 *     and     ob  = { gmap_ob_func = fun () ->
 *                       b0.gmap_b_func oa.gmap_oa_func (p_b.gmap_b_trf) }
 *     in
 *     (p_a, p_b)
 *   in
 *   gmap_myfix (gmap_a0,gmap_b0)
 *
 * (\* extra hacks to skip inherited attribute *\)
 * let gmap_a fa a = fix_result_1.gmap_a_trf  fa a
 * let gmap_b    b = fix_result_2.gmap_b_trf     b
 *
 * let _ =
 *   let show_int = string_of_int in
 *   let t1 = D 5 in
 *   printf "Stage 2\n";
 *   printf "%s\n" @@ show_a show_int @@ t1;
 *   printf "%s\n" @@ show_a show_int @@ gmap_a ((+)1) t1;
 *   ()
 *
 *
 * (\* ***************************** Fmt now ******************************* *\)
 * class ['l, 'self] fmt_a_stub ob fself fl = object
 *   inherit [Format.formatter, 'l, unit, Format.formatter, 'self, unit] a_t
 *   method c_A fmt be = Format.fprintf fmt "A (%a)" (gcata_b (ob ())) be
 *   method c_C fmt    = Format.fprintf fmt "C"
 *   method c_E fmt a  = Format.fprintf fmt "E (%a)" fself a
 *   method c_D fmt l  = Format.fprintf fmt "D (%a)" fl l
 * end
 * class ['self] fmt_b_stub oa fself = object
 *   inherit [Format.formatter, 'self, unit] b_t
 *   method c_I fmt a  =
 *     (\* if type `a` is not used in b than fl doesn't matter
 *        if it is uses we know the type of argument
 *        if it is used with two different types than it is regularity restriction
 *     *\)
 *     let fl fmt = Format.fprintf fmt "%d" in
 *     Format.fprintf fmt "I (%a)"  (gcata_a (oa () fl)) a
 *   method c_J fmt    = Format.fprintf fmt "J"
 *   method c_K fmt b  = Format.fprintf fmt "K (%a)" fself b
 * end
 *
 * type fmt_typ_for_a =
 *   { fmt_a_func :
 *       'a 'selfa 'selfb .
 *         (unit -> (Format.formatter, 'selfb, unit) b_t) ->
 *         (Format.formatter -> 'a a -> unit) ->
 *         (Format.formatter -> 'a -> unit) ->
 *         ('a, 'selfa) fmt_a_stub
 *   }
 * type fmt_typ_for_b =
 *   { fmt_b_func :
 *       'selfa 'selfb .
 *         (unit -> (Format.formatter -> int -> unit) ->
 *          (Format.formatter, int, unit, Format.formatter, 'selfa, unit) a_t) ->
 *       (Format.formatter -> b -> unit) ->
 *       'selfb fmt_b_stub
 *   }
 *
 * type fmt_a_2  = { fmt_a_trf: 'a 'b . (Format.formatter -> 'a -> unit) ->
 *                     Format.formatter -> 'a a -> unit
 *                 }
 * type fmt_b_2  = { fmt_b_trf:          Format.formatter -> b -> unit }
 * type fmt_oa_3 =
 *   { fmt_oa_func:
 *       'a  'selfa .
 *         unit ->
 *       (Format.formatter -> 'a -> unit) ->
 *       (Format.formatter, 'a, unit, Format.formatter, 'selfa, unit) a_t
 *   }
 * type fmt_ob_3 =
 *   { fmt_ob_func: 'selfb . unit -> (Format.formatter, 'selfb, unit) b_t }
 *
 *
 * let (fmt_a0,fmt_b0) = ({ fmt_a_func = new fmt_a_stub}, {fmt_b_func = new fmt_b_stub})
 *
 * let (fix_result_1, fix_result_2) =
 *   let fmt_myfix (a0,b0) =
 *     let rec p_a = { fmt_a_trf = fun fl inh a -> gcata_a (oa.fmt_oa_func () fl) inh a}
 *     and     p_b = { fmt_b_trf = fun inh b -> gcata_b (ob.fmt_ob_func ()) inh b }
 *     and     oa  = { fmt_oa_func = fun () fl ->
 *                       a0.fmt_a_func ob.fmt_ob_func (p_a.fmt_a_trf fl) fl }
 *     and     ob  = { fmt_ob_func = fun () ->
 *                       b0.fmt_b_func oa.fmt_oa_func (p_b.fmt_b_trf) }
 *     in
 *     (p_a, p_b)
 *   in
 *   fmt_myfix (fmt_a0,fmt_b0)
 *
 * (\* extra hacks to skip inherited attribute *\)
 * let fmt_a fa fmt a = fix_result_1.fmt_a_trf fa fmt  a
 * let fmt_b    fmt b = fix_result_2.fmt_b_trf    fmt  b
 *
 * let _ =
 *   let fmt_int fmt n = Format.fprintf fmt "%d" n in
 *   let t1 = D 5 in
 *   Format.printf "Stage 3\n";
 *   Format.printf "%a\n" (fmt_a fmt_int) t1;
 *   Format.printf "%a\n" (fmt_a fmt_int) (A (I C));
 *   Format.printf "%a\n" fmt_b (I (A J));
 *   Format.printf "%a\n" fmt_b (K J);
 *   () *)
