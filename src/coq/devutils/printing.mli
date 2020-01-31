(* Auxiliary functions for printing *)

open Names
open Constr
open Environ
open Evd

(* --- Coq terms --- *)

(* Pretty-print a `global_reference` with fancy `constr` coloring. *)
val pr_global_as_constr : global_reference -> Pp.t

(* Gets a name as a string *)
val name_as_string : Name.t -> string

(* Gets a term as a string in an environment *)
val term_as_string : env -> types -> string

(* --- Coq environments --- *)

(* Gets an environment as a string *)
val env_as_string : env -> string

(* --- Debugging --- *)

(* Print a separator string *)
val print_separator : unit -> unit

(* Debug a term with a descriptor string *)
val debug_term : env -> types -> string -> unit

(* Debug a list of terms with a descriptor string *)
val debug_terms : env -> types list -> string -> unit

(* Debug an environment with a descriptor string *)
val debug_env : env -> string -> unit

(* Print a patch to stdout in the standard Coq format (TODO rename or move) *)
val print_patch : env -> evar_map -> string -> types -> unit
