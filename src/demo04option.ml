open Utils
open Printf

(* ---------------- Original Type ------------------- *)
type 'a t = 'a option = None | Some of 'a

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_toption =
  object
    method virtual c_Some : 'inh -> 'a -> 'syn
    method virtual c_None : 'inh ->       'syn
  end

class ['a] show_toption self fa =
  object
    inherit ['a, unit, string, unit, string] class_toption
    method c_Some () a = sprintf "Some (%a)" fa a
    method c_None ()   = "None"
  end

class ['a, 'a1] map_toption self (fa : unit -> 'a -> 'a1) =
  object
    inherit ['a, unit, 'a1, 'b, 'a1 t] class_toption
    method c_Some () a = Some (fa () a)
    method c_None ()   = None
  end

let rec gcata_toption tr inh t =
  (*let recurse = gcata_t tr in*)
  match t with
  | Some a -> tr#c_Some inh a
  | None   -> tr#c_None inh

let show_toption fa t = fix0 (fun self -> gcata_toption (new show_toption self (fun () -> fa)) ()) t
let gmap_toption fa t = fix0 (fun self -> gcata_toption (new map_toption  self (fun () -> fa)) ()) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n"
    (show_toption id             (Some "1"))
    (show_toption string_of_int  (gmap_toption int_of_string (Some "1")))
