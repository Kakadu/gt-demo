open Utils
open Printf

(* We will implement a show-to-Format.t there *)

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

class virtual ['a, 'ia, 'sa, 'inh, 'syn] class_tree =
  object
    method virtual c_Leaf : 'inh -> 'a -> 'syn
    method virtual c_Node : 'inh -> 'a tree -> 'a -> 'a tree -> 'syn
  end

class ['a] prettify_tree self fa = object
  inherit ['a, Format.formatter, unit, Format.formatter, unit] class_tree
  method c_Leaf fmt x = Format.fprintf fmt "Leaf %a" fa x
  method c_Node fmt l x r =
    Format.fprintf fmt "@[Node@ (%a,@ %a,@ %a)@]" self l fa x self r
end

let rec gcata_tree tr inh t =
  match t with
  | Node (l,x,r) -> tr#c_Node inh l x r
  | Leaf x       -> tr#c_Leaf inh x

let prettify_tree fmt fa t = fix (fun self fmt tree ->
    gcata_tree (new prettify_tree self fa) fmt tree
  ) fmt t


let () =
  let fmt = Format.std_formatter in
  let () =
    let t1 = Node (Leaf 0, 1, Leaf 0) in
    let t2 = Node (t1, 2, t1) in
    let t3 = Node (t2, 3, t2) in
    prettify_tree fmt (fun fmt -> Format.fprintf fmt "%d")  t3
  in
  Format.printf "\n\n%!"


(* Now I want to try show-with-type example *)

open Demo07lists (* logic values are here *)

class ['a] wtfo_logic typ_self self typ_a fa = object
  inherit ['a, unit, string, unit, string] class_logic
  method c_Var   fmt n = sprintf "_.%d : %s" n typ_a
  method c_Value fmt x = fa () x
end

let wtfo_logic typ_a fa t =
  let typ_self = sprintf "%s logic" typ_a in
  fix0 (fun self t ->
    gcata_logic (new wtfo_logic typ_self self typ_a (fun () -> fa)) () t
  ) t

let () =
  printf "show-with-type: %s\n"  (wtfo_logic "int" id (Var 1));
  printf "show-with-type: %s\n"  (wtfo_logic "int" (sprintf "%f") (Value 3.14));
  ()

(* ********************************** Now for logic lists ******************** *)
class ['a, 'b] wtfo_alist typ_self self typ_a fa typ_b fb =
  object
    inherit ['a, unit, string, 'b, unit, string, unit, string] class_alist
    method c_Nil  ()     = "nil"
    method c_Cons () a b = sprintf "cons (%a,%a)" fa a fb b
  end

let wtfo_alist typ_a fa typ_b fb t =
  let typ_self = sprintf "(%s,%s) alist" typ_a typ_b in
  let obj self =
    new wtfo_alist typ_self self
      typ_a (fun () -> fa)
      typ_b (fun () -> fb)
  in
  fix0 (fun self t ->
    gcata_alist (obj self) () t
  ) t

(*  Now for llist *)
class wtfo_llist
    (_:string) self
  = object
  inherit [int, unit, string, unit, string] class_llist
  inherit [(int logic, 'b logic) alist as 'b] wtfo_logic "xxx" (fun () _ -> "3")
      "yyy" (fun () _ -> ""
        (* wtfo_alist "bbb" (fun _ -> "4") *)
        (*   "bbb" (fun _ -> "4")  Nil *)
      )

      (* (fun () l -> wtfo_alist "bbb" (wtfo_logic "ccc" (fa ())) "ddd" (self()) l) *)
end

let _:int = new wtfo_llist

let wtfo_llist typ_a (fa: 'a -> string) (t: 'a llist) =
  let typ_self = sprintf "%s llist" typ_a in
  let o self =
    new wtfo_llist typ_self self typ_a (fun () -> fa)
  in
  fix0 (fun self t ->
    gcata_logic (o (fun () _ -> "")) () t
  ) t

let (_:int) = wtfo_llist
let () =
  printf "%s\n%!" @@ wtfo_llist "int" (sprintf "%d") @@  
    Value (Cons (1, Value Nil))
