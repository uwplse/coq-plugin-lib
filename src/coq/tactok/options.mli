(* --- Options for Decompiler with TacTok --- *)

(* 
 * Beam width is the number of tactics TacTok will predict in order 
 * of probability
 *)

val default_beam_width : int
val set_beam_width : int -> unit
val get_beam_width : unit -> int
