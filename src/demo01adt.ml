open Utils
open Printf

(* ---------------- Original Type ------------------- *)
type ('a, 'b) t = A of 'a | B of 'b

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_t =
  object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end

class ['a, 'b] show_t self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_t
    method c_A () a = sprintf "A (%a)" fa a
    method c_B () b = sprintf "B (%a)" fb b
  end

class ['a, 'a1, 'b, 'b1] map_t self (fa : unit -> 'a -> 'a1) (fb : unit -> 'b -> 'b1) =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) t] class_t
    method c_A () a = A (fa () a)
    method c_B () b = B (fb () b)
  end

let rec gcata_t tr inh t =
  (* let recurse = gcata_t tr in *)
  match t with
  | A a -> tr#c_A inh a
  | B b -> tr#c_B inh b

let show_t fa fb t = fix0 (fun self t -> gcata_t (new show_t self fa fb) () t) t
let gmap_t fa fb t = fix0 (fun self t -> gcata_t (new map_t  self fa fb) () t) t

let _ =
  printf "Original: %s\nMapped: %s\n"
    (show_t (fun () -> id)            (fun () -> id) (A "1"))
    (show_t (fun () -> string_of_int) (fun () -> id) @@
       gmap_t (fun () -> int_of_string) (fun () -> id) (A "1"))

(* -------------------------- First application (reducing the kind) -------------- *)

type 'a tint = ('a, int) t

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_tint =
  object
    inherit ['a, 'ia, 'sa, int, unit, unit, 'inh, 'syn] class_t
  end

class ['a] show_tint self fa =
  object
    inherit ['a, unit, string, unit, string] class_tint
    inherit ['a, int] show_t self fa (fun () -> string_of_int)
  end

class ['a, 'a1] map_tint self fa =
  object
    inherit ['a, unit, 'a1, unit, 'a1 tint] class_tint
    inherit ['a, 'a1, int, int] map_t self fa (fun () -> id)
  end

let gcata_tint : ('a, 'ia, 'sa, 'inh, 'syn) #class_tint -> 'inh -> 'a tint -> 'syn = fun tr inh t -> gcata_t tr inh t

let show_tint fa t = fix0 (fun self t -> gcata_tint (new show_tint self fa) () t) t
let gmap_tint fa t = fix0 (fun self t -> gcata_tint (new map_tint  self fa) () t) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n"
    (show_tint (fun () -> id)               (B 1))
    (show_tint (fun () -> id) (gmap_tint (fun () -> id) (B 1)))

(* -------------------------- Second application (reducing the kind) -------------- *)

type tstringint = string tint

class virtual ['inh, 'syn] class_tstringint =
  object
    inherit [string, unit, unit, 'inh, 'syn] class_tint
  end

class show_tstringint self =
  object
    inherit [unit, string] class_tstringint
    inherit [string] show_tint self (fun () -> id)
  end

class map_tstringint self =
  object
    inherit ['unit, tstringint] class_tstringint
    inherit [string, string] map_tint self (fun () -> id)
  end

let gcata_tstringint : ('inh, 'syn) #class_tstringint -> 'inh -> tstringint -> 'syn =
  fun tr inh t -> gcata_tint tr inh t

let show_tstringint t = fix0 (fun self t ->
    gcata_tstringint (new show_tstringint self) () t
  ) t
let gmap_tstringint t = fix0 (fun self t ->
    gcata_tstringint (new map_tstringint  self) () t
  ) t

let incr2 t = fix0 (fun self t -> gcata_tstringint (object
                                                      inherit map_tstringint self
                                                      method c_B _ x = B (x+1)
                                                    end) () t) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n" (show_tstringint (B 1))
                                             (show_tstringint (incr2 (B 1)))

