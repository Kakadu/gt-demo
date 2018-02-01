module P = struct
  type 'a t = P of 'a
  class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_t = object
    method virtual c_P   : 'inh -> 'a -> 'syn
  end

  let gcata_t tr inh = function
  | P   n -> tr#c_P   inh n

  class ['a] show_t self fa = object
    inherit ['a, unit, string, unit, string] class_t
    method c_P () n = Format.sprintf "P %a" fa n
  end

  let show_t fa () t = Utils.fix0 (fun self -> gcata_t (new show_t self fa)) () t
end

open Demo04option
open P

(* The idea is to put type declaration after class declaration to make
   previous type `t` visible *)
class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_t =
  object
    inherit ['a t, 'ia t, 'sa t, 'inh, 'syn] class_toption
  end
class ['a] show_t self fa =
  object
    inherit ['a, unit, string, unit, string] class_t
    inherit ['a t] show_toption self
        (fun () l -> show_t fa () l)
  end
let gcata_t = gcata_toption
(* will not typecheck *)
let show_t fa t = Utils.fix0 (fun self -> gcata_t (new show_t self fa) ()) t
type nonrec 'a t = 'a t option
let () =
  Printf.printf "%s\n%!" @@ show_t (fun () -> Utils.id) (Some (P "ppp"))

module Experiment2 = struct
  (* Now let's make the same trick with non-alias type *)
  open P

  class virtual ['inh, 'syn] class_t = object
    method virtual c_Foo : 'inh -> int t -> 'syn
  end
  class show_t self = object
    inherit [unit, string] class_t
    method c_Foo () x = Printf.sprintf "Foo(%s)" (show_t (fun () -> string_of_int) () x)
  end
  type nonrec t = Foo of int t
  let gcata_t tr inh = function Foo x -> tr#c_Foo inh x
  let show_t () t = Utils.fix0 (fun self -> gcata_t (new show_t self)) () t
  let () =
    Printf.printf "%s\n%!" @@ show_t ()  (Foo (P 25))
end
