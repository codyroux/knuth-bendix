open Printf
open Types
open Print
open Parse
open Rew
open Narrow

(* E + {s = s}, R  ~> E, R *)
let delete trs =
  let eqs = List.filter (fun e -> e.eq_lhs <> e.eq_rhs) trs.eqns in
  if List.length eqs <> List.length trs.eqns then
    Some { trs with eqns = eqs }
  else None

(* E, R + {s -> t} ~> E, R + {s -> u} if t ->* u *)
let compose trs =
  let rules = trs.rules in
  let rec comp r =
    let rhs = r.r_rhs in
    let all_rules = List.map (fun r -> top_down (apply_head r)) rules in
    let rhs = saturate_step all_rules rhs in
    Option.map (fun rhs -> { r with r_rhs = rhs }) rhs
  in
  let rules = first_of_list comp rules in
  Option.map (fun rules ->
      { trs with rules = rules }
    ) rules

(* E + {s = t}, R ~> E + {s = u}, R if t ->* u *)
let simplify trs = assert false

(* We keep a global precedence *)
let global_prec = ref (fun f g -> 0) (* This is the most permissive
                                        precendence, it only strictly
                                        smaller rhs *)

(* E + {s = t}, R ~> E, R + {s -> t} if s > t *)
let orient trs =
  let eqns = trs.eqns in
  let orient e =
    let lhs = e.eq_lhs in
    let rhs = e.eq_rhs in
    if lpo !global_prec e.eq_lhs e.eq_rhs then
      Some { r_lhs = lhs; r_rhs = rhs }
    else if lpo !global_prec e.eq_rhs e.eq_lhs then
      Some { r_lhs = rhs; r_rhs = lhs }
    else None
  in
  let rec remove_first l acc =
    match l with
    | [] -> None
    | e::es ->
       match orient e with
       | None -> remove_first es (e::acc)
       | Some r -> Some (r, List.rev_append acc es)
  in
  match remove_first eqns [] with
  | None -> None
  | Some (r, eqns) ->
     Some { trs with eqns = eqns; rules = r::trs.rules }

(* E, R + {s -> t} ~> E + {v = t}, R if s "collapses to" v
   see https://homepage.divms.uiowa.edu/~astump/papers/thesis-wehrman.pdf
*)
let collapse trs = assert false

(* E, R ~> E + {s = t}, R if s <- . -> t is a critical pair of R*)
let deduce trs = assert false

(* opportunities for optimization here *)
let rec saturate step_funs trs =
  match saturate_step step_funs trs with
  | None -> trs
  | Some trs' -> saturate step_funs trs'

let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
(* Run with: *)
(* ocamlbuild -r main.native && ./main.native *)
let () =
  glob_in := read_whole_file "group.trs";
  glob_cursor := 0;
  let trs = parse_spec () in
  print_trs stdout trs;
  (* We magically know that this order will do *)
  let test_prec = list_to_prec [["1"];["m"];["i"]] in
  global_prec := test_prec;
  let trs = saturate [delete; orient; compose] trs in
  print_trs stdout trs;
  glob_in := "f(X,Y) -> g(Y,X)";
  glob_cursor := 0;
  let vars = ["X"; "Y"] in
  let rule = parse_rule vars () in
  printf "%a\n" print_rule rule;
  glob_in := "f(f(a,b),f(c,d))";
  glob_cursor := 0;
  let term = parse_term vars () in
  printf "%a\n" print_term term;
  let term0 = apply_head rule term in
  let bottom_up = bottom_up (apply_head rule) in
  let term1 = bottom_up term in
  let top_down = top_down (apply_head rule) in
  let term2 = top_down term in
  let rec repeat f t =
    match f t with
    | None -> t
    | Some t' -> repeat f t'
  in
  printf "%a\n" print_term (Option.get term0);
  printf "%a\n" print_term (Option.get term1);
  printf "%a\n" print_term (Option.get term2);
  printf "%a\n" print_term (repeat bottom_up term);
  printf "%a\n" print_term (repeat top_down term);
  ()
