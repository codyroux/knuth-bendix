open Printf
open Types

let rec print_term out t =
  match t with
  | Var (_, v) -> fprintf out "%s" v
  | App (f, ts) ->
     fprintf out "%s" f;
     if not (List.is_empty ts)
     then
       fprintf out "(%a)" print_term_list ts
     else ()
and print_term_list out ts =
  match ts with
  | [] -> assert false
  | [t] -> print_term out t
  | t::ts -> fprintf out "%a,%a" print_term t print_term_list ts


let print_eqn out eqn =
  fprintf out "%a == %a" print_term eqn.eq_lhs print_term eqn.eq_rhs

let print_rule out rule =
  fprintf out "%a -> %a" print_term rule.r_lhs print_term rule.r_rhs

let print_eqn_list out eqns =
  List.iter (fun e -> fprintf out "%a\n" print_eqn e) eqns

let print_rule_list out rules =
  List.iter (fun r -> fprintf out "%a\n" print_rule r) rules

let print_eqns out eqns =
  fprintf out "(THEORY\n(EQUATIONS\n%a)\n)\n" print_eqn_list eqns

let print_rules out rules =
  fprintf out "(RULES\n%a)\n" print_rule_list rules

let print_vars out vars =
  fprintf out "(VAR %s)\n" (String.concat " " vars)

let print_trs out trs =
  print_vars out trs.vars;
  print_eqns out trs.eqns;
  print_rules out trs.rules;
