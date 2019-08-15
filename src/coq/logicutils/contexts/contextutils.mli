(*
 * Utilities for contexts
 *
 * Many of these functions are thin wrappers around functions from
 * Context.Rel.Declaration or Context.Named.Declaration, so that there 
 * is simple shorthand for calling these functions that is still meaningful
 *)

open EConstr
open Names
open Environ
open Declarations
open Evd

module CRD = Context.Rel.Declaration
module CND = Context.Named.Declaration

(* --- Constructing declarations --- *)

(* 
 * Construct a local assumption/definition 
 *)
val rel_assum : Name.t * types -> (constr, types) CRD.pt
val rel_defin : Name.t * constr * types -> (constr, types) CRD.pt

(*
 * Instantiate a local assumption as a local definition, using the provided term
 * as its definition.
 *
 * Raises an assertion error if the local declaration is not a local assumption.
 *)
val define_rel_decl :
  constr -> (constr, types) CRD.pt -> (constr, types) CRD.pt
                                                               
(*
 * Construct a named assumption/definition
 *)
val named_assum : Id.t * types -> named_declaration
val named_defin : Id.t * constr * types -> named_declaration

(* --- Questions about declarations --- *)

(* 
 * Is the rel declaration a local assumption/definition? 
 *)
val is_rel_assum : rel_declaration -> bool
val is_rel_defin : rel_declaration -> bool

(*
 * Is the named declaration an assumption/definition? 
 *)
val is_named_assum : named_declaration -> bool

(* Is the named declaration a definition? *)
val is_named_defin : named_declaration -> bool

(* --- Deconstructing declarations --- *)

(*
 * Project a component (name, optional value, or type, respectively) of 
 * a rel declaration
 *)
val rel_name : rel_declaration -> Name.t
val rel_value : rel_declaration -> constr option
val rel_type : rel_declaration -> types

(*
 * Project a component (identifier, optional value, or type, respectively) of 
 * a named declaration
 *)
val named_ident : named_declaration -> Id.t
val named_value : named_declaration -> constr option
val named_type : named_declaration -> types

(* --- Mapping over contexts --- *)

(*
 * Map over a rel context with environment kept in synch
 *)
val map_rel_context :
  env -> (env -> rel_declaration -> 'a) -> rel_context -> 'a list

(*
 * Map over a named context with environment kept in synch
 *)
val map_named_context :
  env -> (env -> named_declaration -> 'a) -> named_context -> 'a list     

(* --- Binding in contexts --- *)

(*
 * Bind all local declarations in the relative context onto the body term as
 * products, substituting away (i.e., zeta-reducing) any local definitions.
 *)
val smash_prod_assum : rel_context -> types -> types
val smash_lam_assum : rel_context -> constr -> constr

(*
 * Decompose the first n product bindings, zeta-reducing let bindings to reveal
 * further product/lambda bindings when necessary.
 *)
val decompose_prod_n_zeta : evar_map -> int -> types -> rel_context * types
val decompose_lam_n_zeta : evar_map -> int -> constr -> rel_context * constr

(*
 * Reconstruct local bindings around a term
 *)
val recompose_prod_assum : rel_context -> types -> types
val recompose_lam_assum : rel_context -> types -> types

(* --- Names in contexts --- *)

(*
 * Give a "reasonable" name to each anonymous local declaration in the relative
 * context. Name generation is according to standard Coq policy (cf., Namegen)
 * and does not guarantee freshness, but term type-checking is only sensitive to
 * anonymity. (Names are freshened by subscription when printed.)
 *)
val deanonymize_context :
  env ->
  evar_map ->
  rel_declaration list ->
  rel_declaration list
            
(* --- Getting bindings for certain kinds of terms --- *)

(*
 * Certain terms create bindings you need to push to the environment as you
 * recurse. These functions give you those bindings.
 *)
                                                              
val bindings_for_inductive :
  env -> mutual_inductive_body -> one_inductive_body array -> rel_declaration list

val bindings_for_fix :
  name array -> types array -> rel_declaration list

(* --- Combining contexts --- *) 

(*
 * Append two contexts (inner first, outer second), shifting internal indices.
 *
 * The input contexts are assumed to share the same environment, such that any
 * external indices inside the now-inner context must be shifted to pass over
 * the now-outer context.
 *)
val context_app : rel_context -> rel_context -> rel_context
