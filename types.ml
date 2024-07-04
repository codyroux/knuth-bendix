
type term = Var of int * string | App of string * term list

let var s = Var (0, s)

type eqn = { eq_lhs : term; eq_rhs : term }
type rule = { r_lhs : term; r_rhs : term }

type trs = {
    vars : string list;
    eqns : eqn list;
    rules : rule list;
  }
