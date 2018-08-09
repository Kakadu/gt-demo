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

class virtual [ 'il, 'l, 'sl, 'inh, 'syn ] class_a = object
  method virtual c_A   : 'inh -> b    -> 'syn
  method virtual c_C   : 'inh         -> 'syn
  method virtual c_E   : 'inh -> 'l a -> 'syn
  method virtual c_D   : 'inh ->   'l -> 'syn
end
class virtual [ 'inh, 'syn ] class_b = object
  method virtual c_I   : 'inh -> int a -> 'syn
  method virtual c_J   : 'inh          -> 'syn
  method virtual c_K   : 'inh ->     b -> 'syn
end


(* Test 1: first argument of class is a record *)
(* type ('inh, 'syn) hack_b = { b_hack : unit -> ('inh, 'syn) class_b }
 * type ('inh, 'syn) hack_a = { a_hack : 'l . unit -> (unit -> 'l -> string) -> (unit, 'l, string, 'inh, 'syn) class_a }
 *
 * class ['l] show_a_stub (ob: (unit, string) hack_b) fself fl = object
 *   inherit [unit, 'l, string, unit, string] class_a
 *   method c_A () be = sprintf "A (%a)" (gcata_b @@ ob.b_hack ()) be
 *   method c_C ()    = "C"
 *   method c_E () a  = sprintf "E (%a)" fself a
 *   method c_D () l  = sprintf "D (%a)" fl l
 * end
 * class show_b_stub (oa: (unit, string) hack_a) fself = object
 *   inherit [unit, string] class_b
 *   method c_I () a  =
 *     (\* if type `a` is not used in b than fl doesn't matter
 *        if it is uses we know the type of argument
 *        if it is used with two different types than it is regularity restriction
 *     *\)
 *     let fl () _ =  assert false in
 *     sprintf "I (%a)" (gcata_a @@ oa.a_hack () fl) a
 *   method c_J ()    = "J"
 *   method c_K () b  = sprintf "K (%a)" fself b
 * end
 *
 *
 * let (a0,b0) = (new show_a_stub, new show_b_stub)
 *
 *
 * let myfix () =
 *   let rec show_a fl inh a = gcata_a (oa () fl) inh a
 *   and     oa () fl = { a_hack = (fun () -> a0 ob __ fl) }
 *   and     ob ()    = { b_hack = (fun () -> b0 oa show_b)         }
 *   and     show_b inh b = gcata_b (ob ()) inh b
 *   in
 *   (show_a, show_b)
 *
 *
 * let (show_a, show_b) = myfix () *)



(* Test 2 *)

class ['l] show_a_stub ob fself fl = object
  inherit [unit, 'l, string, unit, string] class_a
  method c_A () be = sprintf "A (%a)" (gcata_b @@ ob ()) be
  method c_C ()    = "C"
  method c_E () a  = sprintf "E (%a)" fself a
  method c_D () l  = sprintf "D (%a)" fl l
end
class show_b_stub oa fself = object
  inherit [unit, string] class_b
  method c_I () a  =
    (* if type `a` is not used in b than fl doesn't matter
       if it is uses we know the type of argument
       if it is used with two different types than it is regularity restriction
    *)
    let fl () _ =  assert false in
    sprintf "I (%a)" (gcata_a @@ oa () fl) a
  method c_J ()    = "J"
  method c_K () b  = sprintf "K (%a)" fself b
end

type typ_for_a = { a_func :
                     'a . (unit -> (unit, string) class_b) ->
                     (unit -> 'a a -> string) ->
                     (unit -> 'a -> string) ->
                     'a show_a_stub
                 }
type typ_for_b = { b_func :
                     'a . (unit -> (unit -> int -> string) ->
                           (unit, int, string, unit, string) class_a) ->
                     (unit -> b -> string) ->
                     show_b_stub
                 }
let (a0,b0) = ({ a_func = new show_a_stub}, {b_func = new show_b_stub})
type a_trf  = { a_trf: 'a . (unit -> 'a -> string) -> unit -> 'a a -> string }
type b_trf  = { b_trf:      unit -> b -> string }
type oa_func= { oa_func: 'a . unit -> (unit -> 'a -> string) ->
                              (unit, 'a, string, unit, string) class_a }
type ob_func= { ob_func: unit -> (unit, string) class_b }


let myfix (a0,b0) =
  let rec show_a = { a_trf = fun fl inh a -> gcata_a (oa.oa_func () fl) inh a }
  and     oa     = { oa_func = fun () fl -> a0.a_func ob.ob_func (show_a.a_trf fl) fl }
  and     ob     = { ob_func = fun ()    -> b0.b_func oa.oa_func show_b.b_trf }
  and     show_b = { b_trf = (fun inh b -> gcata_b (ob.ob_func ()) inh b) }
  in
  (show_a, show_b)

let (fix_result_1, fix_result_2) = myfix (a0,b0)
(* let (show_a, show_b) =
 *   let {a_trf},{b_trf} = myfix (a0,b0) in
 *   a_trf,b_trf *)

let show_a fa a = fix_result_1.a_trf fa a
let show_b    b = fix_result_2.b_trf b

let _ =
  (* let show_string () s = s in *)
  let show_int    () n = string_of_int n in
  printf "%s\n" @@ show_a show_int () (E C);
  printf "%s\n" @@ show_a show_int () (A (I C));
  printf "%s\n" @@ show_b () (I (A J));
  printf "%s\n" @@ show_b () (K J);
  ()
