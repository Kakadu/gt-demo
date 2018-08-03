(* mutal recursion *)
open Utils
open Printf

type a = A of b | C | E of a
and  b = I of a | J | K of b

let rec gcata_a tr inh = function
  | A a -> tr#c_A inh a
  | C   -> tr#c_C inh
  | E a -> tr#c_E inh a
let rec gcata_b tr inh = function
  | I b -> tr#c_I inh b
  | J   -> tr#c_J inh
  | K b -> tr#c_K inh b

class virtual [ 'inh, 'syn ] class_a = object
  method virtual c_A   : 'inh -> b -> 'syn
  method virtual c_C   : 'inh      -> 'syn
  method virtual c_E   : 'inh -> a -> 'syn
end
class virtual [ 'inh, 'syn ] class_b = object
  method virtual c_I   : 'inh -> a -> 'syn
  method virtual c_J   : 'inh      -> 'syn
  method virtual c_K   : 'inh -> b -> 'syn
end

class show_a_stub (ob: unit -> (_,_) #class_b) fself = object
  inherit [unit, string] class_a
  method c_A () be = sprintf "A (%a)" (gcata_b @@ ob()) be
  method c_C ()    = "C"
  method c_E () a  = sprintf "E (%a)" fself a
end
class show_b_stub (oa: unit -> (_,_) #class_a) fself = object
  inherit [unit, string] class_b
  method c_I () a  = sprintf "I (%a)" (gcata_a @@ oa ()) a
  method c_J ()    = "J"
  method c_K () b  = sprintf "K (%a)" fself  b
end

let __ = Obj.magic ()

let ttt1 = (new show_a_stub, new show_b_stub)
let myfix (a0, b0) =
  let rec show_a inh a = gcata_a (oa ()) inh a
  and     oa () = a0 ob show_a
  and     ob () = b0 oa show_b
  and     show_b inh b = gcata_b (ob ()) inh b
  in
  (show_a, show_b)


let (show_a, show_b) = myfix ttt1

let _ =
  printf "%s\n" @@ show_a () (E C);
  printf "%s\n" @@ show_a () (A (I C));
  printf "%s\n" @@ show_b () (I (A J));
  printf "%s\n" @@ show_b () (K J);
  ()
