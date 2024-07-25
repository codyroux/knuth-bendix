val delete : Types.trs -> Types.trs option
val compose : Types.trs -> Types.trs option
val simplify : Types.trs -> Types.trs option
val global_prec : (string -> string -> int) ref
val orient : Types.trs -> Types.trs option
val collapse : 'a -> 'b
val deduce : Types.trs -> Types.trs option
val read_whole_file : string -> string
