
type term = Var of string | App of string * term list

type eqn = { eq_lhs : term; eq_rhs : term }
type rule = { r_lhs : term; r_rhs : term }

type trs = {
    vars : string list;
    eqns : eqn list;
    rules : unit;
  }
