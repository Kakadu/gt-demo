open Utils
open Printf

(* ---------------- Original Type ------------------- *)
type ('a, 'b) t = A of 'a | B of 'b

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_t = object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end

class ['a, 'b] show_t fa fb fself =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_t
    method c_A () a = sprintf "A (%a)" (fun () -> fa) a
    method c_B () b = sprintf "B (%a)" (fun () -> fb) b
  end

class ['a, 'a1, 'b, 'b1] gmap_t (fa : 'a -> 'a1) (fb : 'b -> 'b1) fself =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) t] class_t
    method c_A () a = A (fa a)
    method c_B () b = B (fb b)
  end

let rec gcata_t tr inh t =
  match t with
  | A a -> tr#c_A inh a
  | B b -> tr#c_B inh b

let t = { gcata = gcata_t; plugins = object end }

let show_t fa fb s = transform(t) (new show_t fa fb) s
let gmap_t fa fb s = transform(t) (new gmap_t fa fb) s

let _ =
  printf "Original: %s\nMapped: %s\n"
    (show_t id            id (A "1"))
    (show_t show_int id @@
       gmap_t (int_of_string) (id) (A "1"))

(* -------------------------- First application (reducing the kind) -------------- *)

type 'a tint = ('a, int) t

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_tint =
  object
    inherit ['a, 'ia, 'sa, int, unit, unit, 'inh, 'syn] class_t
  end

class ['a] show_tint fa fself = object
  inherit ['a, unit, string, unit, string] class_tint
  inherit ['a, int] show_t fa show_int fself
end

class ['a, 'a1] gmap_tint fa fself = object
    inherit ['a, unit, 'a1, unit, 'a1 tint] class_tint
    inherit ['a, 'a1, int, int] gmap_t fa id fself
  end

let gcata_tint : ('a, 'ia, 'sa, 'inh, 'syn) #class_tint -> 'inh -> 'a tint -> 'syn = fun tr inh t -> gcata_t tr inh t
let tint = { gcata = gcata_tint; plugins = object end }

let show_tint fa s = transform(t) (new show_tint fa) s
(* fix0 (fun self t -> gcata_tint (new show_tint self fa) () t) t *)
let gmap_tint fa s = transform(t) (new gmap_tint fa) s
(* fix0 (fun self t -> gcata_tint (new map_tint  self fa) () t) t *)

let _ =
  Printf.printf "Original: %s\nMapped: %s\n"
    (show_tint id               (B 1))
    (show_tint id (gmap_tint id (B 1)))

(* -------------------------- Second application (reducing the kind) -------------- *)

type tstringint = string tint

class virtual ['inh, 'syn] class_tstringint = object
  inherit [string, unit, unit, 'inh, 'syn] class_tint
end

class show_tstringint fself = object
    inherit [unit, string] class_tstringint
    inherit [string] show_tint id fself
  end

class gmap_tstringint fself = object
  inherit ['unit, tstringint] class_tstringint
  inherit [string, string] gmap_tint id fself
end

let gcata_tstringint : ('inh, 'syn) #class_tstringint -> 'inh -> tstringint -> 'syn =
  fun tr inh t -> gcata_tint tr inh t

let tstringint = { gcata = gcata_tstringint; plugins = object end }
let show_tstringint s = transform(tstringint) (new show_tstringint) s
(* fix0 (fun self t ->
 *     gcata_tstringint (new show_tstringint self) () t
 *   ) t *)
let gmap_tstringint s = transform(tstringint) (new gmap_tstringint) s
(* fix0 (fun self t ->
 *     gcata_tstringint (new map_tstringint  self) () t
 *   ) t *)

let incr2 s =
  transform(tstringint)
    (fun fself -> object
       inherit gmap_tstringint fself
       method c_B _ x = B (x+1)
    end)
    s
(* fix0 (fun self t -> gcata_tstringint (object
 *                                                       inherit map_tstringint self
 *                                                       method c_B _ x = B (x+1)
 *                                                     end) () t) t *)

let _ =
  Printf.printf "Original: %s\nMapped: %s\n" (show_tstringint (B 1))
                                             (show_tstringint (incr2 (B 1)))

