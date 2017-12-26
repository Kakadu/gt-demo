(** gmap from fold (The stuff that Pottier couldn't do without penalty) *)
open Utils
open Demo04option

(* first of all we need foldl for option *)

class ['a, 'syn] foldl_toption self (fa: 'syn -> 'a -> 'syn) =
  object
    inherit ['a, 'syn, 'syn, 'syn, 'syn] class_toption
    method c_None inh  = inh
    method c_Some inh a = fa inh a
  end

(* Is needed because inherited and synthesized attributes become mixed *)
type ('a,'b) either = Left of 'a | Right of 'b

(* we construct a function during map using fold and in the end
   we will apply it to the initial value
*)
class ['a, 'b] map_option1 self (fa: _ -> _ -> _) = object
  inherit ['a, (unit, 'b) either -> 'b option] foldl_toption
      (fun _inh _opt -> assert false)
      (fun _inh a -> assert false)
  method! c_None inh   = fun _ -> None
  method! c_Some inh a = fa (function Right b -> Some b) a
end


let my_map fa t =
  fix0 (fun self t ->
    gcata_toption
      (new map_option1 self (fun inh a -> fun _ -> inh (Right (fa a))))
      (fun _ -> None)  (* init *)
      t
      (Left())   (* we apply result to the initial value *)
  ) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n"
    (show_toption id             (Some "06"))
    (show_toption string_of_int  (my_map int_of_string (Some "06")))

