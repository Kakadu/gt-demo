open Printf

type z = Z
type 'a s = S : 'a -> 'a s

type ('a, 'size) vector =
  | Nil : ('a, z) vector
  | Cons : 'a * ('a, 'prev) vector -> ('a, 'prev s) vector

let vector_gcata: type size .
      < c_Nil : 'inh -> 'syn
      ; c_Cons : 'size_prev . 'inh -> 'a -> ('a, 'size_prev) vector -> 'syn > ->
    'inh -> ('a, size) vector -> 'syn =
fun tr inh -> function
| Nil -> tr#c_Nil inh
| Cons (h, tl) -> tr#c_Cons inh h tl

let length : type size . ('a, size) vector -> int = fun _ -> 1

let rec show_easy: type size . (unit -> 'a -> string) -> ('a, size) vector -> string =
  fun (type size) fa (xs : ('a, size) vector) ->
  match xs with
  | Nil -> "[]"
  | Cons (h, tl) -> sprintf "%a :: %s" fa h (show_easy fa tl)

class virtual ['ia, 'a, 'sa, 'inh, 'self, 'syn] vector_t = object
  method virtual c_Nil  : 'inh ->  'syn
  method virtual c_Cons : 'size . 'inh -> 'a -> ('a, 'size) vector -> 'syn
end

class ['a, 'self] show_vector (* : (type size) . (unit -> 'a -> string) -> ('a, size) vector -> _  *)
   : 'size . (unit -> 'a -> string) -> ('a, 'size) vector -> _
  = fun fa fself ->
    object
      inherit [unit, 'a, string, unit, 'self, string] vector_t
      method c_Nil inh = "[]"
      method c_Cons : 'size . unit -> 'a -> ('a, 'size) vector -> 'syn =
        fun (type size) () h (tl : ('a, size) vector) ->
          Printf.sprintf "%s :: %s" (fa () h) (fself () tl)
    end

open Utils

let () =
  print_endline @@
  GT.transform_gc vector_gcata
    (new show_vector (GT.lift string_of_int))
    ()
    (Cons (3, Cons (2, Nil)))
