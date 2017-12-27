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
  prettify_tree fmt (fun fmt -> Format.fprintf fmt "%d") @@
  let t1 = Node (Leaf 0, 1, Leaf 0) in
  let t2 = Node (t1, 2, t1) in
  let t3 = Node (t2, 3, t2) in
  t3


(* Now I want to try show-with-type example *)
