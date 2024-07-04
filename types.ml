
type var = { level : int; name : string }

type term = Var of var | App of string * term list

let var s = Var { level = 0; name = s }

type eqn = { eq_lhs : term; eq_rhs : term }
type rule = { r_lhs : term; r_rhs : term }

type trs = {
    vars : string list;
    eqns : eqn list;
    rules : rule list;
  }
