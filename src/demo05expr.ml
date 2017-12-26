(* from https://github.com/dboulytchev/GT/blob/master/regression/test004.ml *)
open Utils
open Printf

(* mutal recursion *)
type a = [`A of b | `C of int]
and  b = [`B of a list | `D of string]

and 'self a_open =
  'self constraint 'self = [> `A of b_open | `C of int]
and 'self b_open =
  'self constraint 'self = [> `B of a_open list | `D of string]


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
