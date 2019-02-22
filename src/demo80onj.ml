type ('a, 'b) t =
  | A of ('b, 'a) t
  | B of 'a

class ['a, 'b, 'ta, 'tb] m =
  object
    method t : ('a -> 'ta) -> ('b -> 'tb) -> ('a, 'b) t -> ('ta, 'tb) t =
      fun fa fb s ->
      match s with
      | A bat ->
        let x: ('tb, 'ta) t = (new m)#t fb fa bat in
        A  x
      | B  a       -> B (fa a)
  end

let rec tf : 'a 'b 'tb 'ta . ('a -> 'ta) -> ('b -> 'tb) -> ('a, 'b) t -> ('ta, 'tb) t =
      fun fa fb s ->
      match s with
      | A bat ->
        let x = tf fb fa bat in
        A x
      | B  a       -> B (fa a)
