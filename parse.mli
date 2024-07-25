val glob_in : string ref
val glob_cursor : int ref
exception Parse_Error of string * int
val eof : unit -> bool
val peek : unit -> char
val pop : unit -> char
val is_space : char -> bool
val is_alphanum : char -> bool
val parse_ident : unit -> string
val parse_space : unit -> unit
val parse_char : char -> unit
val parse_key : string -> unit
val paren : (unit -> 'a) -> 'a
val comma : unit -> unit
val parse_list : (unit -> 'a) -> (unit -> 'b) -> 'b list
val parse_vars : unit -> string list
val parse_arg_list : string list -> unit -> Types.term list
val parse_term : string list -> unit -> Types.term
val parse_equation : string list -> unit -> Types.eqn
val parse_rule : string list -> unit -> Types.rule
val parse_rules : string list -> unit -> Types.rule list
val parse_theory : string list -> unit -> Types.eqn list
val parse_theories : string list -> unit -> Types.eqn list
val parse_spec : unit -> Types.trs
