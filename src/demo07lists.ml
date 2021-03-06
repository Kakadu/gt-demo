open Utils
open Printf

(* --------------------------------- List workout --------------------------------- *)

type ('a, 'b) alist = Nil | Cons of 'a * 'b

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_alist = object
  method virtual c_Nil  : 'inh -> 'syn
  method virtual c_Cons : 'inh -> 'a -> 'b -> 'syn
end

class ['a, 'b] show_alist fa fb fself = object
  inherit ['a, unit, string, 'b, unit, string, unit, string] class_alist
  method c_Nil  ()     = "nil"
  method c_Cons () a b = sprintf "cons (%s,%s)" (fa () a) (fb () b)
end

class ['a, 'a1, 'b, 'b1] gmap_alist fa fb fself = object
  inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) alist] class_alist
  method c_Nil  _     = Nil
  method c_Cons _ a b = Cons (fa () a, fb () b)
end

let rec gcata_alist tr inh t = match t with
  | Nil         -> tr#c_Nil inh
  | Cons (a, b) -> tr#c_Cons inh a b

let alist = { gcata = gcata_alist
            ; fix = (fun c -> GT.transform_gc gcata_alist c)
            }

let show_alist fa fb t = GT.transform(alist) (new show_alist fa fb) () t

let gmap_alist fa fb t = GT.transform(alist) (new gmap_alist fa fb) () t

let _ =
  printf "Original: %s\n" @@
  show_alist (lift id) (lift string_of_int) (Cons ("a", 1));
  printf "Original: %s\n" @@
  show_alist (lift id) (lift string_of_int) @@
  gmap_alist (fun () x -> x^"1") (fun () -> (+)1) (Cons ("a", 1))

(* --------------------------------- Recursion! --------------------------------- *)

type 'a list = ('a, 'a list) alist

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_list = object
  inherit ['a, 'is, 'sa, 'a list, 'ia list, 'sa list, 'inh, 'syn] class_alist
end

class ['a] show_list fa fself = object
  inherit ['a, unit, string, unit, string] class_list
  inherit ['a, 'a list] show_alist fa fself fself
end
class ['a, 'b] gmap_list fa fself = object
  inherit ['a, unit, 'b, unit, 'b list] class_list
  inherit ['a, 'b, 'a list, 'b list] gmap_alist fa fself fself
end

let gcata_list = gcata_alist

let list = { gcata = alist.gcata
           ; fix = (fun c -> GT.transform_gc gcata_list c)
           }

let show_list fa  t = GT.transform(list) (new show_list fa) () t

let gmap_list fa  t = GT.transform(list) (new gmap_list fa) () t

let () =
  Printf.printf "Original: %s\n" @@
  show_list (lift show_int) (Cons (1, Cons (2, Nil)))

(* ----------------------------------- Logic workout ------------------------------ *)

type 'a logic = Var of int | Value of 'a

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_logic = object
  method virtual c_Var   : 'inh -> int -> 'syn
  method virtual c_Value : 'inh -> 'a  -> 'syn
end

class ['a] show_logic fa fself = object
  inherit ['a, unit, string, unit, string] class_logic
  method c_Var   _ n = "." ^ string_of_int n
  method c_Value _ n = fa n
  end

class ['a, 'a1] gmap_logic fa fself =
  object
    inherit ['a, unit, 'a1, unit, 'a1 logic] class_logic
    method c_Var   () n = Var n
    method c_Value () n = Value (fa () n)
  end

let gcata_logic tr inh t =
  match t with
  | Var   n -> tr#c_Var   inh n
  | Value n -> tr#c_Value inh n

let logic = { gcata = gcata_logic
            ; fix = (fun c -> GT.transform_gc gcata_logic c)
            }
let show_logic fa  t = GT.fix(logic) (new show_logic fa) () t
let gmap_logic fa  t = GT.fix(logic) (new gmap_logic fa) () t

(* let show_logic fa t = fix0 (fun self t -> gcata_logic (new show_logic self fa) () t) t
 * let gmap_logic fa t = fix0 (fun self t -> gcata_logic (new gmap_logic self fa) () t) t *)

let _ = printf "Original: %s\nMapped: %s\n"
    (show_logic id (Value "a"))
    (show_logic id (gmap_logic (fun () x -> x ^ "!") (Value "a")))

(* -------------------------------- Logical lists ------------------------------ *)

type 'a llist = ('a logic, 'a llist) alist logic

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_llist =
  object
    inherit [('a logic, 'a llist) alist, ('ia logic, 'ia llist) alist, ('sa logic, 'sa llist) alist, 'inh, 'syn] class_logic
  end

class ['a] show_llist fa fself =
  object
    inherit ['a, unit, string, unit, string] class_llist
    inherit [('a logic, 'a llist) alist] show_logic
        (fun l -> show_alist (fun () -> show_logic fa) fself l)
        fself
  end

class ['a, 'b] gmap_llist fa fself  = object
  inherit ['a, unit, 'b logic, unit, 'b llist] class_llist
  inherit [('a logic, 'a llist) alist, ('b logic, 'b llist) alist] gmap_logic
        (fun () l -> gmap_alist (fun () -> gmap_logic fa) fself l)
        fself
end

let gcata_llist tr inh t = gcata_logic tr inh t
let llist = { gcata = gcata_llist
            ; fix = (fun c -> GT.transform_gc gcata_logic c)
            }

let show_llist fa  t = GT.transform(llist) (new show_llist fa) () t
let gmap_llist fa  t = GT.transform(llist) (new gmap_llist fa) () t


let () =
  printf "Original: %s\n"
    (show_llist id (Value (Cons (Value "a", Value (Cons (Value "b", Value Nil))))));
  printf "Original: %s"
    (show_llist id (gmap_llist (fun () x -> x ^ "!")
                      (Value (Cons (Value "a", Value (Cons (Value "b", Value Nil)))))))
