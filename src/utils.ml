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

let show_int = string_of_int
let show_string = Printf.sprintf "%S"
let unused _ _ = failwith "*** Using the unused ***"
