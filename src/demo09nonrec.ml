open Utils

module P = struct
  type 'a t = P of 'a
  class virtual ['ia, 'a, 'sa, 'inh, 'syn] class_t = object
    method virtual c_P   : 'inh -> 'a -> 'syn
  end

  let gcata_t tr inh = function P   n -> tr#c_P   inh n

  class ['a] show_t fa fself = object
    inherit [unit, 'a, string, unit, string] class_t
    method c_P () n = Printf.sprintf "P %a" fa n
  end

  let t = { Utils.gcata = gcata_t
          ; fix = (fun c -> GT.transform_gc gcata_t c)
          }
  let show_t fa s = GT.transform(t) (new show_t fa) () s
end

open Demo04option
open P

(* The idea is to put type declaration after class declaration to make
   previous type `t` visible *)
class virtual ['ia, 'a, 'sa, 'inh, 'syn] class_t = object
  inherit ['ia t, 'a t, 'sa t, 'inh, 'syn] class_toption
end
class ['a] show_t fa fself = object
  inherit [ unit, 'a, string, unit, string] class_t
  inherit ['a t] show_toption
      (fun () l -> show_t fa l)
      fself
end

let t = toption
(* let show_t fa t = Utils.fix0 (fun self -> gcata_t (new show_t self fa) ()) toption *)
let show_t fa s = GT.transform(t) (new show_t fa) () s

(* for type alias we can always put definition after constructor *)
type nonrec 'a t = 'a t option
let () = Printf.printf "%s\n%!" @@ show_t (lift id) (Some (P "ppp"))

module Experiment2 = struct
  (* Now let's make the same trick with non-alias type *)
  open P

  class virtual ['inh, 'syn] class_t = object
    method virtual c_Foo : 'inh -> int t -> 'syn
  end
  class show_t fself = object
    inherit [unit, string] class_t
    method c_Foo () x = Printf.sprintf "Foo(%s)" (show_t (lift string_of_int) x)
  end
  type nonrec t = Foo of int t
  (* for definitions of algebraic nonrec datatypes which shadows other type
     we onliged to put type definition before gcata
  *)
  let gcata_t tr inh = function Foo x -> tr#c_Foo inh x
  let t = { Utils.gcata = gcata_t
           ; fix = (fun c -> GT.transform_gc gcata_t c)
          }
  let show_t s = GT.transform(t) (new show_t) () s

  let () = Printf.printf "%s\n%!" @@ show_t (Foo (P 25))

  (* now let's try to extend generated class *)
  class attempt_show self = object
    inherit show_t self
    method! c_Foo () x =
      (* it's not obvious what canwe do with the value of shadowed type but it will be
         a end-user's issue *)
      ">..<"
  end

  let attempt_show () t = Utils.fix0 (fun self -> gcata_t (new attempt_show self)) () t
  let () =
    Printf.printf "%s\n%!" @@ attempt_show () (Foo (P 77))

end
