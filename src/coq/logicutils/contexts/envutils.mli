(*
 * Utility functions for environments
 * TODO organize
 *)

open Environ
open Constr
open Names
open Evd
open Stateutils

(* Look up all indexes from a list in an environment *)
val lookup_rels : int list -> env -> rel_declaration list

(* Return a list of all bindings in an environment, starting with the closest *)
val lookup_all_rels : env -> rel_declaration list
                                                       
(* Return a list of all indexes in an environment, starting with 1 *)
val all_rel_indexes : env -> int list

(* Return a list of relative indexes, from highest to lowest, of size n *)
val mk_n_rels : int -> types list

(* Return a name-type pair from the given rel_declaration. *)
val rel_name_type : rel_declaration -> Name.t * types

(*
 * Push to an environment
 *)
val push_local : (name * types) -> env -> env
val push_let_in : (name * types * types) -> env -> env

(*
 * Lookup from an environment
 *)
val lookup_pop : int -> env -> (env * rel_declaration list)
val lookup_definition : env -> types -> types
val unwrap_definition : env -> types -> types

(*
 * Offset between an environment and an index, or two environments, respectively
 *)
val new_rels : env -> int -> int
val new_rels2 : env -> env -> int

(*
 * Ignore the environment and evar_map in a function
 *)
val ignore_env : ('a -> 'b) -> (env -> evar_map -> 'a -> 'b)

                                 
(* Finds all rel names pushed onto an environment. *)
val get_pushed_names : env -> Id.Set.t
  
(* If the given name is anonymous, generate a fresh one. *)
val fresh_name : env -> Name.t -> Id.t

                                    
(* Returns true if the relative bindings in each environment
   are syntactically equal. *)
val compare_envs : env -> env -> evar_map -> bool state
