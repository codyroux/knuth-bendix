
type term = Var of bool * string | App of string * term list

let var s = Var (false, s)

type eqn = { eq_lhs : term; eq_rhs : term }
type rule = { r_lhs : term; r_rhs : term }

type trs = {
    vars : string list;
    eqns : eqn list;
    rules : rule list;
  }
