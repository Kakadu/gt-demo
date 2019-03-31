(* Demo 14: mutal recursion with single fix  *)
open Utils
open Printf

let __ = Obj.magic ()

type    'a a = A of b     | C | B of 'a
and     b    = I of int a | J

let rec gcata_a tr inh = function
  | A a -> tr#c_A inh a
  | B x -> tr#c_B inh x
  | C   -> tr#c_C inh

let rec gcata_b tr inh = function
  | I b -> tr#c_I inh b
  | J   -> tr#c_J inh

class virtual [ 'ia, 'a, 'sa, 'inh, 'self, 'syn ] a_t = object
  method virtual c_A   : 'inh -> b    -> 'syn
  method virtual c_B   : 'inh -> 'a   -> 'syn
  method virtual c_C   : 'inh         -> 'syn
end
class virtual [ 'inh, 'self, 'syn ] b_t = object
  method virtual c_I   : 'inh ->   int a -> 'syn
  method virtual c_J   : 'inh            -> 'syn
end

(* Final transformation function here *)
(* if 'sr is a type parameter we will  restrict it too much because of b *)
(* type ('s, 'sr) a_trf  = { a_trf: 'r     . ('r -> 'sr) -> 'r a -> 's } *)
type    a_trf  = { a_trf: 'r 'sr 's . ('r -> 'sr) -> 'r a -> 's }
type    b_trf  = { b_trf:        's .                   b -> 's }
(* 's can't be a parameter because class show for a will instantiate this parameter
   to be always a string *)

class ['a, 'self] show_a_stub for_a for_b ~fself fa = object
  inherit [unit, 'a, string, unit, 'self, string] a_t
  method c_A () be = sprintf "A (%s)" (for_b.b_trf be)
  method c_B () x  = sprintf "B (%s)" (fa x)
  method c_C ()    = "C"
end
class ['self] show_b_stub for_a for_b ~fself = object
  inherit [unit, 'self, string] b_t
  method c_I () a  = sprintf "I (%s)" (for_a.a_trf (sprintf "%d") a)
  method c_J ()    = "J"
end

type ('inh, 'sa, 'sb, 's, 'sr) typ_for_a =
  { a_func : 'selfa 'r .
               a_trf -> b_trf -> fself:('r a -> 's) ->
               ('r -> 'sr) ->
      ('inh, 'r, 'sa, 'inh, 'selfa, 's) a_t
  }
type ('sa, 'sr, 'sb, 'inh, 's) typ_for_b =
  { b_func : 'self .
                a_trf -> b_trf -> fself:(b -> 's) ->
      (('inh, 'self, 's) b_t)
  }

let (a0,b0) =
  ( {a_func = new show_a_stub}
  , {b_func = new show_b_stub}
  )

let myfix (a0,b0) =
  let rec trait_a = { a_trf = fun f a ->
      gcata_a (a0.a_func trait_a trait_b (trait_a.a_trf f) f) () a
    }
  and     trait_b = { b_trf = fun    b ->
      gcata_b (b0.b_func trait_a trait_b  trait_b.b_trf  ) () b }
  in
  (trait_a, trait_b)

let (fix_result_1, fix_result_2) = myfix (a0,b0)

let show_a    a = fix_result_1.a_trf    a
let show_b    b = fix_result_2.b_trf    b

let _ =
  let show_int = string_of_int in
  printf "Stage 1\n";
  (* printf "%s\n" @@ show_a    (E C); *)
  printf "%s\n" @@ show_a show_int (A (I C));
  printf "%s\n" @@ show_b          (I (A J));
  ()


(* Reimplementing some stuff *)

class ['self] show_a_stub2 for_a for_b ~fself fa = object
  inherit [ 'self] show_a_stub for_a for_b ~fself fa
  method! c_A () be = sprintf "A {%a}" (fun () -> for_b.b_trf) be
end

let (a0,b0) =
  ( {a_func = new show_a_stub2}
  , {b_func = new show_b_stub}
  )

let (fix_result_1, fix_result_2) = myfix (a0,b0)

class ['self] show_a fself fa = object
  inherit ['self] show_a_stub2 fix_result_1 fix_result_2 fself fa
end
class ['self] show_b fself = object
  inherit ['self] show_b_stub fix_result_1 fix_result_2  ~fself
end
let show_a    s = fix_result_1.a_trf    s
let show_b    s = fix_result_2.b_trf    s

let _ =
  printf "Stage 2 (a reimplemented):\n";
  let show_int n = string_of_int n in
  printf "%s\n" @@ show_a  show_int C;
  printf "%s\n" @@ show_a  show_int (A (I C));
  printf "%s\n" @@ show_b (I (A J));
  printf "%s\n" @@ show_b (J);
  ()

(** gmap *********************************************** *)

(* type gmap_a_trf  = { gmap_a_trf: 'c 'd .    a ->    a }
 * type gmap_b_trf  = { gmap_b_trf:            b ->    b } *)


class ['r, 'r2, 'self] gmap_a_stub for_a for_b ~fself fa = object
  inherit [unit, 'self, 'r2 a] a_t
  method c_A () be = A (for_b.b_trf be)
  method c_C ()    = C
end
class ['self] gmap_b_stub for_a for_b ~fself = object
  inherit [unit, 'self, b] b_t
  method c_I () a  = I (for_a.a_trf id a)
  method c_J ()    = J
end

(* type ('inh, 'syn) gmap_typ_for_a =
 *   { gmap_a_func : 'self .
 *         gmap_a_trf -> gmap_b_trf ->
 *         fself:(a -> a) ->
 *         ('inh, 'self, 'syn) a_t
 *   }
 * type ('inh, 'syn) gmap_typ_for_b =
 *   { gmap_b_func : 'self .
 *       gmap_a_trf -> gmap_b_trf ->
 *       fself:(b -> b) ->
 *       ('inh, 'self, 'syn) b_t
 *   } *)

let (gmap_a0,gmap_b0) =
  ( { a_func = new gmap_a_stub}
  , { b_func = new gmap_b_stub}
  )

let (fix_result_1, fix_result_2) = myfix (gmap_a0, gmap_b0)
(* let (fix_result_1, fix_result_2) =
 *   let gmap_myfix (a0,b0) =
 *     let rec gmap_a = { gmap_a_trf = fun s ->
 *         (\* Eta-expansion matters !!! *\)
 *         gcata_a (a0.gmap_a_func gmap_a gmap_b  (gmap_a.gmap_a_trf)) () s }
 *     and     gmap_b = { gmap_b_trf = fun s ->
 *         gcata_b (b0.gmap_b_func gmap_a gmap_b gmap_b.gmap_b_trf) () s
 *       }
 *     in
 *     (gmap_a, gmap_b)
 *   in
 *   gmap_myfix (gmap_a0,gmap_b0) *)

(* extra hacks to skip inherited attribute *)
let gmap_a fa s = fix_result_1.a_trf fa  s
let gmap_b    s = fix_result_2.b_trf     s

let _ =
  (* let show_int = string_of_int in *)
  let t1 : int a = A (I C) in
  let t2 : float a = A (I C) in
  printf "Stage 2\n";
  printf "%s\n" @@ show_a id @@ t1;
  printf "%s\n" @@ show_a id @@ t2;
  printf "%s\n" @@ show_a id @@ gmap_a id t1;
  ()

(* ******************  Let's reuse type a as an alias ************ *)
(* type 'a ccc = 'a a
 * let gcata_ccc = gcata_a
 * class virtual ['ia,'a,'sa, 'inh, 'self, 'syn] ccc_t = object
 *   inherit ['ia,'a,'sa, 'inh, 'self, 'syn] a_t
 * end
 * class ['a, 'selfc] show_ccc fself fa = object
 *   inherit ['a, 'selfc] show_a fself fa
 * end
 * let show_ccc fa c =
 *   fix0 (fun fself -> gcata_ccc (new show_ccc fself fa) ()) c
 * let _ =
 *   let show_int n = string_of_int n in
 *   printf "Testing 'a c:\n%!";
 *   printf "%s\n" @@ show_ccc show_int (E C);
 *   printf "%s\n" @@ show_ccc show_int (A (I C));
 *   ()
 *
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
