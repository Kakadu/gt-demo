(* mutal recursion *)
open Utils
open Printf


type a = A of b | C
and  b = B of a | D


class virtual [ 'inh, 'syn ] class_a = object
  method virtual c_A   : 'inh -> b -> 'syn
  method virtual c_C   : 'inh      -> 'syn
end
class virtual [ 'inh, 'syn ] class_b = object
  method virtual c_B   : 'inh -> a -> 'syn
  method virtual c_D   : 'inh      -> 'syn
end

class show_a_stub self for_b = object
  inherit [unit, string] class_a
  method c_A () be = sprintf "A (%a)" for_b be
  method c_C ()    = "C"
end
class show_b_stub self for_a = object
  inherit [unit, string] class_b
  method c_B () a  = sprintf "B (%a)" for_a a
  method c_D ()    = "D"
end
let rec gcata_a tr inh t = match t with
  | A a -> tr#c_A inh a
  | C   -> tr#c_C inh
let rec gcata_b tr inh t = match t with
  | B b -> tr#c_B inh b
  | D   -> tr#c_D inh

let rec show_a t = fix0 (fun self ->
  gcata_a (new show_a_stub self (fun () -> show_b)) ()
) t
and show_b t = fix0 (fun self ->
  gcata_b (new show_b_stub self (fun () -> show_a)) ()
) t

class show_a self = object
  inherit show_a_stub self (fun () -> show_b)
end
class show_b self = object
  inherit show_b_stub self (fun () -> show_a)
end
let _ =
  printf "%s\n" @@ show_a (A (B C));
  printf "%s\n" @@ show_b (B (A D));
  ()

(* class show_a self = object *)

(* We can't use functions to show b in class a declaration
   So the right way is to split definition making them more abstract
*)
(*class virtual ['self, 'iself, 'sself, 'polyvar_extra, 'inh, 'syn] class_a =
  object
    method virtual c_A   : 'inh -> b   -> 'syn
    method virtual c_C   : 'inh -> int -> 'syn
  end

class ['a] show_a self fself =
  object
    inherit ['self, unit, string, 'polyvar_extra, unit, string] class_a
    method c_Var ()   s = s
    method c_Const () b = sprintf "%d" b
    method c_Binop () _ s x y = "(" ^ (x.fx ()) ^ s ^ (y.fx ()) ^ ")"
  end

class ['a] eval s =
  object
    inherit ['a, unit, int, unit, int] t_t
    method c_Var   _ _ x       = s x
    method c_Const _ _ n       = n
    method c_Binop _ _ f _ x y = f (x.fx ()) (y.fx ())
end*)
