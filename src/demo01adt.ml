open Utils
open Printf

(* ---------------- Original Type ------------------- *)
type ('a, 'b) t = A of 'a | B of 'b

class virtual ['ia, 'a, 'sa, 'ib, 'b, 'sb, 'inh, 'self, 'syn] class_t = object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end

class ['a, 'b] show_t fa fb fself =
  object
    inherit [unit, 'a, string, unit, 'b, string, unit, _, string] class_t
    method c_A () a = sprintf "A (%a)" fa a
    method c_B () b = sprintf "B (%a)" fb b
  end

class ['a, 'a1, 'b, 'b1] gmap_t (fa : unit -> 'a -> 'a1) (fb : _ -> 'b -> 'b1) fself =
  object
    inherit [unit, 'a, 'a1, unit, 'b, 'b1, unit, ('a,'b) t, ('a1, 'b1) t] class_t
    method c_A () a = A (fa () a)
    method c_B () b = B (fb () b)
  end

let rec gcata_t tr inh t =
  match t with
  | A a -> tr#c_A inh a
  | B b -> tr#c_B inh b

let t = { gcata = gcata_t
        ; fix = (fun c -> GT.transform_gc gcata_t c)
        }

let show_t fa fb s = GT.transform(t) (new show_t fa fb) () s
let gmap_t fa fb s = GT.transform(t) (new gmap_t fa fb) () s

let _ =
  printf "Original: %s\nMapped: %s\n"
    (show_t (lift id)           (lift id) (A "1"))
    (show_t (lift show_int) (lift  id) @@
       gmap_t (lift int_of_string) (lift id) (A "1"))

(* -------------------------- First application (reducing the kind) -------------- *)

type 'a tint = ('a, int) t

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] class_tint =
  object
    inherit ['ia, 'a, 'sa, unit, int, int, 'inh, 'self, 'syn] class_t
  end

class ['a, 'self] show_tint fa fself = object
  inherit [unit, 'a, string, unit, 'self, string] class_tint
  inherit ['a, int] show_t fa (lift show_int) fself
end

class ['a, 'a1] gmap_tint fa fself = object
    inherit [unit, 'a, 'a1, unit, 'a tint, 'a1 tint] class_tint
    inherit ['a, 'a1, int, int] gmap_t fa (lift id) fself
  end

let gcata_tint : ('ia, 'a, 'sa, 'inh, 'self, 'syn) #class_tint -> 'inh -> 'a tint -> 'syn = fun tr inh t -> gcata_t tr inh t
let tint = { gcata = gcata_tint
           ; fix = (fun c -> GT.transform_gc gcata_tint c)
           }

let show_tint fa s = GT.transform(t) (new show_tint fa) () s
(* fix0 (fun self t -> gcata_tint (new show_tint self fa) () t) t *)
let gmap_tint fa s = GT.transform(t) (new gmap_tint fa) () s
(* fix0 (fun self t -> gcata_tint (new map_tint  self fa) () t) t *)

let _ =
  Printf.printf "Original: %s\nMapped: %s\n"
    (show_tint (lift id)                      (B 1))
    (show_tint (lift id) (gmap_tint (lift id) (B 1)))

(* -------------------------- Second application (reducing the kind) -------------- *)

type tstringint = string tint

class virtual ['inh, 'syn] class_tstringint = object
  inherit [unit, string, string, unit, 'inh, 'syn] class_tint
end

class show_tstringint fself = object
    inherit [unit, string] class_tstringint
    inherit [string, tstringint] show_tint (lift id) fself
  end

class gmap_tstringint fself = object
  inherit ['unit, tstringint] class_tstringint
  inherit [string, string] gmap_tint (lift id) fself
end

let gcata_tstringint : ('inh, 'syn) #class_tstringint -> 'inh -> tstringint -> 'syn =
  fun tr inh t -> gcata_tint tr inh t

let tstringint = { gcata = gcata_tstringint
                 ; fix = (fun c -> GT.transform_gc gcata_tstringint c)
                 }

let show_tstringint s = GT.transform(tstringint) (new show_tstringint) () s
let gmap_tstringint s = GT.transform(tstringint) (new gmap_tstringint) () s

let incr2 s =
  GT.transform(tstringint)
    (fun fself -> object
       inherit gmap_tstringint fself
       method c_B _ x = B (x+1)
     end)
    ()
    s

let _ =
  Printf.printf "Original: %s\nMapped: %s\n" (show_tstringint (B 1))
                                             (show_tstringint (incr2 (B 1)))

