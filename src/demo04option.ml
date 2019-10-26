open Utils
open Printf

(* ---------------- Original Type ------------------- *)
type 'a t = 'a option = None | Some of 'a

class virtual ['ia, 'a, 'sa, 'inh, 'syn] class_toption = object
  method virtual c_Some : 'inh -> 'a -> 'syn
  method virtual c_None : 'inh ->       'syn
end

class ['a] show_toption fa _ =
  object
    inherit [unit, 'a, string, unit, string] class_toption
    method c_Some () a = sprintf "Some (%s)" (fa () a)
    method c_None ()   = "None"
  end

class ['a, 'a1] gmap_toption (fa : unit ->  'a -> 'a1) _ =
  object
    inherit [unit, 'a, 'a1, 'b, 'a1 t] class_toption
    method c_Some () a = Some (fa () a)
    method c_None ()   = None
  end

let gcata_toption tr inh t = match t with
  | Some a -> tr#c_Some inh a
  | None   -> tr#c_None inh

let toption = { gcata = gcata_toption
              ; fix = (fun c -> GT.transform_gc gcata_toption c)
              }

let show_toption fa s = GT.transform(toption) (new show_toption fa) () s
let gmap_toption fa s = GT.transform(toption) (new gmap_toption fa) () s


let _ =
  Printf.printf "Original: %s\n"
    (show_toption (lift id)             (Some "1"));
  Printf.printf "Mapped: %s\n" @@
  show_toption (lift string_of_int)  @@
  gmap_toption (lift int_of_string) (Some "1")
