open Printf
open Types

let glob_in = ref ""

let glob_cursor = ref 0

exception Parse_Error of string * int

let eof () = !glob_cursor >= String.length !glob_in

let peek () = !glob_in.[!glob_cursor]

let pop () = let c = peek() in incr glob_cursor; c

let is_space c = c = ' ' || c = '\n'

(* don't over think it... *)
let is_alphanum c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

let parse_ident () =
  if eof () || not (is_alphanum (peek ())) then
    begin
      let err = sprintf "Expected alphanum at %d" (!glob_cursor) in
      raise (Parse_Error (err, !glob_cursor))
    end;
  let pos = !glob_cursor in
  let len = ref 0 in
  while (not (eof ())) && is_alphanum (peek ()) do
    ignore (pop ());
    incr len;
  done;
  String.sub !glob_in pos !len

let parse_space () =
  while (not (eof ())) && is_space (peek ()) do
    ignore (pop ());
  done

let parse_char c =
  if (not (eof ())) && (peek () = c) then ignore (pop ())
  else
    let err = sprintf "Expected %c at position %d" c !glob_cursor in
    raise (Parse_Error (err, !glob_cursor))

let parse_key key =
  let pos = !glob_cursor in
  let str_pos = ref 0 in
  while !str_pos < String.length key && pop () = key.[!str_pos] do
    incr str_pos;
  done;
  if !str_pos != String.length key then
    begin
      let err = sprintf "Expected %s at position %d" key pos in
      raise (Parse_Error (err, pos))
    end;
  parse_space ()

let paren p =
  parse_char '(';
  let v = p () in
  parse_char ')';
  v

let comma () = parse_char ','

let rec parse_list sep p =
  try
    let v = p () in
    try
      sep ();
      v :: parse_list sep p
    with
    | Parse_Error _ -> [v]
  with
  | Parse_Error _ -> []


let parse_vars () =
  let vars () =
    parse_key "VAR";
    parse_list parse_space parse_ident
  in
  paren vars

let rec parse_arg_list vars () =
  let args () = parse_list comma (parse_term vars) in
  paren args
and parse_term vars () =
  let f = parse_ident () in
  if not (eof ()) && peek () = '(' then
    let args = parse_arg_list vars () in
    App(f, args)
  else if List.mem f vars then
    Var(f)
  else App(f, [])

let parse_equation vars () =
  let t1 = parse_term vars () in
  parse_key " == ";
  let t2 = parse_term vars () in
  { eq_lhs = t1; eq_rhs = t2}

let parse_rule vars () =
  let t1 = parse_term vars () in
  parse_key " -> ";
  let t2 = parse_term vars () in
  { r_lhs = t1; r_rhs = t2}

let parse_rules vars () =
  let rules () =
    parse_key "RULES";
    parse_list parse_space (parse_rule vars)
  in
  paren rules

let parse_theory vars () =
  let eqns () =
    parse_key "EQUATIONS";
    parse_list parse_space (parse_equation vars)
  in
  paren eqns

let parse_theories vars () =
  let theory () =
    parse_key "THEORY";
    parse_list parse_space (parse_theory vars)
  in
  let ret = paren theory in
  List.hd ret

let parse_spec () =
  let vars = parse_vars () in
  parse_space ();
  let theory = parse_theories vars () in
  parse_space ();
  let rules = parse_rules vars () in
  { vars = vars; eqns = theory; rules = rules }
