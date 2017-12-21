let lift f _ = f
let id x     = x

let fix0 f t =
  let knot = ref (fun _ -> assert false) in
  let recurse t = f !knot t in
  knot := recurse;
  recurse t

let fix f inh t =
  let knot = ref (fun _ -> assert false) in
  let recurse inh t = f !knot inh t in
  knot := recurse;
  recurse inh t

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
    method c_A () a = "A (" ^ fa a ^ ")"
    method c_B () b = "B (" ^ fb b ^ ")"
  end

class ['a, 'a1, 'b, 'b1] map_t self (fa : 'a -> 'a1) (fb : 'b -> 'b1) =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) t] class_t
    method c_A () a = A (fa a)
    method c_B () b = B (fb b)
  end

let rec gcata_t tr inh t =
  let recurse = gcata_t tr in
  match t with
  | A a -> tr#c_A inh a
  | B b -> tr#c_B inh b

let show_t fa fb t = fix0 (fun self t -> gcata_t (new show_t self fa fb) () t) t
let gmap_t fa fb t = fix0 (fun self t -> gcata_t (new map_t  self fa fb) () t) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n" (show_t id            id (A "1"))
                                             (show_t string_of_int id (gmap_t int_of_string id (A "1")))

(* -------------------------- First application (reducing the kind) -------------- *)

type 'a tint = ('a, int) t

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_tint =
  object
    inherit ['a, 'ia, 'sa, int, unit, unit, 'inh, 'syn] class_t
  end

class ['a] show_tint self fa =
  object
    inherit ['a, unit, string, unit, string] class_tint
    inherit ['a, int] show_t self fa string_of_int
  end

class ['a, 'a1] map_tint self fa =
  object
    inherit ['a, unit, 'a1, unit, 'a1 tint] class_tint
    inherit ['a, 'a1, int, int] map_t self fa id
  end

let gcata_tint : ('a, 'ia, 'sa, 'inh, 'syn) #class_tint -> 'inh -> 'a tint -> 'syn = fun tr inh t -> gcata_t tr inh t

let show_tint fa t = fix0 (fun self t -> gcata_tint (new show_tint self fa) () t) t
let gmap_tint fa t = fix0 (fun self t -> gcata_tint (new map_tint  self fa) () t) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n" (show_tint id               (B 1))
                                             (show_tint id (gmap_tint id (B 1)))

(* -------------------------- Second application (reducing the kind) -------------- *)

type tstringint = string tint

class virtual ['inh, 'syn] class_tstringint =
  object
    inherit [string, unit, unit, 'inh, 'syn] class_tint
  end

class show_tstringint self =
  object
    inherit [unit, string] class_tstringint
    inherit [string] show_tint self id
  end

class map_tstringint self =
  object
    inherit ['unit, tstringint] class_tstringint
    inherit [string, string] map_tint self id
  end

let gcata_tstringint : ('inh, 'syn) #class_tstringint -> 'inh -> tstringint -> 'syn =
  fun tr inh t -> gcata_tint tr inh t

let show_tstringint t = fix0 (fun self t -> gcata_tstringint (new show_tstringint self) () t) t
let gmap_tstringint t = fix0 (fun self t -> gcata_tstringint (new map_tstringint  self) () t) t

let incr2 t = fix0 (fun self t -> gcata_tstringint (object
                                                      inherit map_tstringint self
                                                      method c_B _ x = B (x+1)
                                                    end) () t) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n" (show_tstringint (B 1))
                                             (show_tstringint (incr2 (B 1)))

(* -------------------------- Second application (increasing the kind) -------------- *)

type ('a, 'b) tpairint = ('a * 'b) tint

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_tpairint =
  object
    inherit [('a * 'b), ('ia * 'ib), ('sa * 'sb), 'inh, 'syn] class_tint
  end

class ['a, 'b] show_tpairint self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_tpairint
    inherit [('a * 'b)] show_tint self (fun (x, y) -> "(" ^ fa x ^ ", " ^ fb y ^ ")")
  end

class ['a, 'a1, 'b, 'b1] map_tpairint self fa fb =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) tpairint] class_tpairint
    inherit [('a * 'b), ('a1 * 'b1)] map_tint self (fun (x, y) -> (fa x, fb y))
  end

let gcata_tpairint : ('a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn) #class_tpairint -> 'inh -> ('a, 'b) tpairint -> 'syn =
  fun tr inh t -> gcata_tint tr inh t

let show_tpairint fa fb t = fix0 (fun self t -> gcata_tpairint (new show_tpairint self fa fb) () t) t
let gmap_tpairint fa fb t = fix0 (fun self t -> gcata_tpairint (new map_tpairint  self fa fb) () t) t

let _ =
  Printf.printf "Original: %s\nMapped: %s\n" (show_tpairint string_of_int id (A (1, "2")))
                                             (show_tpairint string_of_int id (gmap_tpairint (fun x -> x+1) id (A (1, "2"))))

(* --------------------------------- List workout --------------------------------- *)

type ('a, 'b) alist = Nil | Cons of 'a * 'b

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'inh, 'syn] class_alist =
  object
    method virtual c_Nil  : 'inh -> 'syn
    method virtual c_Cons : 'inh -> 'a -> 'b -> 'syn
  end

class ['a, 'b] show_alist self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_alist
    method c_Nil  _     = "nil"
    method c_Cons _ a b = "cons (" ^ fa a ^ ", " ^ fb b ^ ")"
  end

class ['a, 'a1, 'b, 'b1] map_alist self fa fb =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1, unit, ('a1, 'b1) alist] class_alist
    method c_Nil  _     = Nil
    method c_Cons _ a b = Cons (fa a, fb b)
  end

let rec gcata_alist tr inh t =
  let recurse inh t = gcata_alist tr inh t in
  match t with
  | Nil         -> tr#c_Nil inh
  | Cons (a, b) -> tr#c_Cons inh a b

let show_alist fa fb t = fix0 (fun self t -> gcata_alist (new show_alist self fa fb) () t) t
let gmap_alist fa fb t = fix0 (fun self t -> gcata_alist (new map_alist  self fa fb) () t) t

let _ = Printf.printf "Original: %s\nMapped: %s\n" (show_alist id string_of_int (Cons ("a", 1)))
                                                   (show_alist id string_of_int (gmap_alist (fun x -> x ^ "1") (fun x -> x+1) (Cons ("a", 1))))

(* --------------------------------- Recursion! --------------------------------- *)

type 'a list = ('a, 'a list) alist

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_list =
  object
    inherit ['a, 'is, 'sa, 'a list, 'ia list, 'sa list, 'inh, 'syn] class_alist
  end

class ['a] show_list self fa =
  object
    inherit ['a, unit, string, unit, string] class_list
    inherit ['a, 'a list] show_alist self fa self
  end

let rec gcata_list tr inh t = gcata_alist tr inh t

(* Version with regular fix:

 let show_list fa t = fix (fun self _ t -> gcata_list (new show_list (self ()) fa) () t) () t

*)

let show_list fa t = fix0 (fun self t -> gcata_list (new show_list self fa) () t) t

let _ = Printf.printf "Original: %s\n" (show_list string_of_int (Cons (1, Cons (2, Nil))))

(* ----------------------------------- Logic workout ------------------------------ *)

type 'a logic = Var of int | Value of 'a

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_logic =
  object
    method virtual c_Var   : 'inh -> int -> 'syn
    method virtual c_Value : 'inh -> 'a  -> 'syn
  end

class ['a] show_logic self fa =
  object
    inherit ['a, unit, string, unit, string] class_logic
    method c_Var   _ n = "." ^ string_of_int n
    method c_Value _ n = fa n
  end

class ['a, 'a1] map_logic self fa =
  object
    inherit ['a, unit, 'a1, unit, 'a1 logic] class_logic
    method c_Var   _ n = Var n
    method c_Value _ n = Value (fa n)
  end

let gcata_logic tr inh t =
  match t with
  | Var   n -> tr#c_Var   inh n
  | Value n -> tr#c_Value inh n

let show_logic fa t = fix0 (fun self t -> gcata_logic (new show_logic self fa) () t) t
let gmap_logic fa t = fix0 (fun self t -> gcata_logic (new map_logic  self fa) () t) t

let _ = Printf.printf "Original: %s\nMapped: %s\n" (show_logic id (Value "a"))
                                                   (show_logic id (gmap_logic (fun x -> x ^ "!") (Value "a")))

(* -------------------------------- Logical lists ---------------------------------------- *)

type 'a llist = ('a logic, 'a llist) alist logic

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_llist =
  object
    inherit [('a logic, 'a llist) alist, ('ia logic, 'ia llist) alist, ('sa logic, 'sa llist) alist, 'inh, 'syn] class_logic
  end

class ['a] show_llist self fa =
  object
    inherit ['a, unit, string, unit, string] class_llist
    inherit [('a logic, 'a llist) alist] show_logic self (fun l -> show_alist (show_logic fa) self l)
  end

class ['a, 'b] map_llist self fa =
  object
    inherit ['a, unit, 'b logic, unit, 'b llist] class_llist
    inherit [('a logic, 'a llist) alist, ('b logic, 'b llist) alist] map_logic self (fun l -> gmap_alist (gmap_logic fa) self l)
  end

let gcata_llist tr inh t = gcata_logic tr inh t

let show_llist fa t = fix0 (fun self t -> gcata_llist (new show_llist self fa) () t) t
let gmap_llist fa t = fix0 (fun self t -> gcata_llist (new map_llist  self fa) () t) t

let _ = Printf.printf "Original: %s\nMapped: %s\n" (show_llist id (Value (Cons (Value "a", Value (Cons (Value "b", Value Nil))))))
                                                   (show_llist id (gmap_llist (fun x -> x ^ "!") (Value (Cons (Value "a", Value (Cons (Value "b", Value Nil)))))))

(* ************************************************* *)

type ('a, 'b) pv = [ `A of 'a | `B of 'b ]
and ('self,'a,'b) pv_open =
  'self constraint 'self = [> `A of 'a  | `B of 'b ]


class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'polyvar_extra, 'inh, 'syn] class_pv =
  object
    method virtual c_A : 'inh -> 'a -> 'syn
    method virtual c_B : 'inh -> 'b -> 'syn
  end

class ['a, 'b] show_pv self fa fb =
  object
    inherit ['a, unit, string, 'b, unit, string, 'polyvar_extra, unit, string] class_pv
    method c_A () a = "`A (" ^ fa a ^ ")"
    method c_B () b = "`B (" ^ fb b ^ ")"
  end

(* I added an extra argument for the 'self variable in the open type
 * We need this in gmap class and not in a show implementation
*)
class ['a, 'a1, 'b, 'b1, 'polyvar_extra] map_pv
    self (fa : 'a -> 'a1) (fb : 'b -> 'b1) =
  object
    inherit [ 'a, unit, 'a1, 'b, unit, 'b1
            , 'polyvar_extra, unit, 'polyvar_extra] class_pv
    method c_A () a = `A (fa a)
    method c_B () b = `B (fb b)
  end

let rec gcata_pv tr inh p =
  match p with
  | `A a -> tr#c_A inh a
  | `B b -> tr#c_B inh b

let show_pv fa fb t = fix0 (fun self t -> gcata_pv (new show_pv self fa fb) () t) t
let gmap_pv fa fb t = fix0 (fun self t -> gcata_pv (new map_pv  self fa fb) () t) t

let _ =
  Printf.printf "Original PV: %s\nMapped PV: %s\n"
      (show_pv id            id (`A "1"))
      (show_pv string_of_int id (gmap_pv int_of_string id (`A "1")))


(* ********************************************************************************************** *)
type ('a, 'b) pv_ext = [ `C of 'a | ('a, 'b) pv ]
and ('self,'a,'b) pv_ext_open =
  'self constraint 'self = [> `C of 'a  | ('a, 'b) pv ]

class virtual ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'polyvar_extra, 'inh, 'syn] class_pv_ext =
  object
    inherit ['a, 'ia, 'sa, 'b, 'ib, 'sb, 'polyvar_extra, 'inh, 'syn] class_pv
    method virtual c_C : 'inh -> 'a -> 'syn
  end
class ['a, 'b] show_pv_ext self fa fb =
  object
    inherit [ 'a, unit, string, 'b, unit, string
            , ('a, 'b) pv_ext, unit, string] class_pv_ext
    inherit ['a, 'b] show_pv self fa fb
    method c_C () a = "`A (" ^ fa a ^ ")"
  end

class ['a, 'a1, 'b, 'b1, 'polyvar_extra] map_pv_ext
          self (fa : 'a -> 'a1) (fb : 'b -> 'b1) =
  object
    inherit ['a, unit, 'a1, 'b, unit, 'b1
            , 'polyvar_extra, unit, ('polyvar_extra, 'a1, 'b1) pv_ext_open
            ] class_pv_ext
    inherit ['a, 'a1, 'b, 'b1, 'polyvar_extra ] map_pv self fa fb
    method c_C () a = `C (fa a)
  end

let rec gcata_pv_ext tr inh p =
  match p with
  | `C a -> tr#c_C inh a
  | #pv as subj -> gcata_pv tr inh subj

let show_pv_ext fa fb t =
  fix0 (fun self t -> gcata_pv_ext
    (new show_pv_ext self fa fb) () t) t

let gmap_pv_ext fa fb t = fix0 (fun self t -> gcata_pv_ext (new map_pv_ext self fa fb) () t) t

let _ =
  Printf.printf "Original pv: %s\n"
      (show_pv id            id (`A "1"));
  Printf.printf "Mapped pv and showed as a pv_ext: %s\n"
    (show_pv_ext string_of_int id
      (gmap_pv int_of_string id (`A "1")));
  Printf.printf "Original pv_ext: %s\n"
    (show_pv_ext id            id (`C "1"));
  Printf.printf "Mapped PV_ext and showed as a pv_ext: %s\n"
    (show_pv_ext string_of_int id (gmap_pv_ext int_of_string id (`C "1")));
