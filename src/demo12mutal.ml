(* mutal recursion *)
open Utils
open Printf

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

class ['l] show_a_stub (ob: unit -> (_,_) #class_b) fself fl = object
  inherit [unit, 'l, string, unit, string] class_a
  method c_A () be = sprintf "A (%a)" (gcata_b @@ ob()) be
  method c_C ()    = "C"
  method c_E () a  = sprintf "E (%a)" fself a
  method c_D () l  = sprintf "D (%a)" fl l
end
class show_b_stub (oa: unit -> (unit -> 'l -> string) -> (_,'l,_,_,_) #class_a) fself = object
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

(* let (_:int) = new show_b_stub *)
let __ = Obj.magic ()

let ttt1 = ((fun x y z -> new show_a_stub x y z), (fun x y -> new show_b_stub x y))


let myfix (a0, b0) =
  let rec show_a fl inh a = gcata_a (oa () fl) inh a
  and     oa () fl = a0 ob (show_a fl) fl
  and     ob ()    = b0 oa show_b
  and     show_b inh b = gcata_b (ob ()) inh b
  in
  (show_a, show_b)


let (show_a, show_b) = myfix ttt1

(* let (_:int) = show_a *)

let _ =
  let show_string () s = s in
  let show_int    () n = string_of_int n in
  printf "%s\n" @@ show_a show_int () (E C);
  printf "%s\n" @@ show_a show_int () (A (I C));
  printf "%s\n" @@ show_b () (I (A J));
  printf "%s\n" @@ show_b () (K J);
  ()
