type expr = ..

class virtual ['inh, 'self, 'syn] expr_t = object

end

type expr +=
  | Const of int
  | Add of expr * expr

class ['inh, 'self, 'syn] expr_t = object
  inherit ['inh, 'self, 'syn] expr_t
end
