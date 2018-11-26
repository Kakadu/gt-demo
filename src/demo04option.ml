open Utils
open Printf

(* ---------------- Original Type ------------------- *)
type 'a t = 'a option = None | Some of 'a

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_toption = object
  method virtual c_Some : 'inh -> 'a -> 'syn
  method virtual c_None : 'inh ->       'syn
end

class ['a] show_toption fa _ =
  object
    inherit ['a, unit, string, unit, string] class_toption
    method c_Some () a = sprintf "Some (%s)" (fa a)
    method c_None ()   = "None"
  end

class ['a, 'a1] gmap_toption (fa : 'a -> 'a1) _ =
  object
    inherit ['a, unit, 'a1, 'b, 'a1 t] class_toption
    method c_Some () a = Some (fa a)
    method c_None ()   = None
  end

let rec gcata_toption tr inh t =
  (*let recurse = gcata_t tr in*)
  match t with
  | Some a -> tr#c_Some inh a
  | None   -> tr#c_None inh

let toption = { gcata = gcata_toption }

let show_toption fa s = transform(toption) (new show_toption fa) s
let gmap_toption fa s = transform(toption) (new gmap_toption fa) s


let _ =
  Printf.printf "Original: %s\nMapped: %s\n"
    (show_toption id             (Some "1"))
    (show_toption string_of_int  (gmap_toption int_of_string (Some "1")))
