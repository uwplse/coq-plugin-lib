(*
 * Utilities for contexts
 *
 * Many of these functions are thin wrappers around functions from
 * Context.Rel.Declaration, so that there is simple shorthand for calling
 * these functions that is still meaningful
 *)

open Constr
open Names
open Environ
open Declarations

module CRD = Context.Rel.Declaration

(* --- Constructing declarations --- *)

(* 
 * Construct a local assumption 
 *)
val rel_assum : Name.t * 'types -> ('constr, 'types) CRD.pt

(*
 * Construct a local definition
 *)
val rel_defin : Name.t * 'constr * 'types -> ('constr, 'types) CRD.pt

(* --- Questions about declarations --- *)

(* 
 * Is the rel declaration a local assumption/definition? 
 *)
val is_rel_assum : ('constr, 'types) CRD.pt -> bool
val is_rel_defin : ('constr, 'types) CRD.pt -> bool

(* --- Deconstructing declarations --- *)

(*
 * Project a component (name, optional value, or type, respectively) of 
 * a rel declaration
 *)
val rel_name : ('constr, 'types) CRD.pt -> Name.t
val rel_value : ('constr, 'types) CRD.pt -> 'constr option
val rel_type : ('constr, 'types) CRD.pt -> 'types

(* --- Mapping over contexts --- *)

(*
 * Map over a rel context with environment kept in synch
 *)
val map_rel_context : env -> (env -> CRD.t -> 'a) -> Context.Rel.t -> 'a list

(* --- Binding in contexts --- *)

(*
 * Bind all local declarations in the relative context onto the body term as
 * products, substituting away (i.e., zeta-reducing) any local definitions.
 *)
val smash_prod_assum : Context.Rel.t -> types -> types

(*
 * Bind all local declarations in the relative context onto the body term as
 * lambdas, substituting away (i.e., zeta-reducing) any local definitions.
 *)
val smash_lam_assum : Context.Rel.t -> constr -> constr

(*
 * Decompose the first n product bindings, zeta-reducing let bindings to reveal
 * further product bindings when necessary.
 *)
val decompose_prod_n_zeta : int -> types -> Context.Rel.t * types

(*
 * Decompose the first n lambda bindings, zeta-reducing let bindings to reveal
 * further lambda bindings when necessary.
 *)
val decompose_lam_n_zeta : int -> constr -> Context.Rel.t * constr

(* --- Getting bindings for certain kinds of terms --- *)

(*
 * Certain terms create bindings you need to push to the environment as you
 * recurse. These functions give you those bindings.
 *)
                                                              
val bindings_for_inductive :
  env -> mutual_inductive_body -> one_inductive_body array -> CRD.t list

val bindings_for_fix :
  name array -> types array -> CRD.t list
