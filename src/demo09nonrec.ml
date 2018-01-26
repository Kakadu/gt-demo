module P = struct
  type 'a t = P of 'a
  class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_t = object
    method virtual c_P   : 'inh -> 'a -> 'syn
  end

  let gcata_t tr inh t = function
  | P   n -> tr#c_P   inh n

  class ['a] show_t self fa = object
    inherit ['a, unit, string, unit, string] class_t
    method c_P () n = "P " ^ (fa n)
  end

  let show_t fa t = Utils.fix0 (fun self -> gcata_t (new show_t self fa) ()) t
end

open Demo04option
open P

(* AIM: to generate something for type nonrec 'a t = 'a t option *)

(* and now it is either impossible to generate  code .... *)
(* type nonrec 'a t = 'a t option
 *
 * class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_t =
 *   object
 *     inherit ['a P.t, 'ia P.t, 'sa P.t, 'inh, 'syn] class_toption
 *     (* by default we use 'a t and not 'a P.t in the line above *)
 *   end
 * class ['a] show_t self fa =
 *   object
 *     inherit ['a, unit, string, unit, string] class_t
 *     inherit ['a P.t] show_toption self
 *         (fun l -> show_t fa l)
 *   end *)


(* ... or we can think that for aliases we can generate classes _before_ actual type
   declaration but it will no type check *)

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_t =
  object
    inherit ['a t, 'ia t, 'sa t, 'inh, 'syn] class_toption
  end
class ['a] show_t self fa =
  object
    inherit ['a, unit, string, unit, string] class_t
    inherit ['a t] show_toption self
        (fun l -> show_t fa l)
  end
let gcata_t = gcata_t
(* will not typecheck *)
let show_t fa t = Utils.fix0 (fun self -> gcata_t (new show_t self fa) ()) t
type nonrec 'a t = 'a t option
let () =
  Printf.printf "%s\n%!" @@ show_t id (Some (P "ppp"))
