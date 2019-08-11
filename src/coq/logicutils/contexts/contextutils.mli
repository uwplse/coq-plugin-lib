(*
 * Utilities for contexts
 *
 * Many of these functions are thin wrappers around functions from
 * Context.Rel.Declaration or Context.Named.Declaration, so that there 
 * is simple shorthand for calling these functions that is still meaningful
 *)

open Constr
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
val rel_assum : Name.t * 'types -> ('constr, 'types) CRD.pt
val rel_defin : Name.t * 'constr * 'types -> ('constr, 'types) CRD.pt

(*
 * Construct a named assumption/definition
 *)
val named_assum : Id.t * 'types -> ('constr, 'types) CND.pt
val named_defin : Id.t * 'constr * 'types -> ('constr, 'types) CND.pt

(* --- Questions about declarations --- *)

(* 
 * Is the rel declaration a local assumption/definition? 
 *)
val is_rel_assum : ('constr, 'types) CRD.pt -> bool
val is_rel_defin : ('constr, 'types) CRD.pt -> bool

(*
 * Is the named declaration an assumption/definition? 
 *)
val is_named_assum : ('constr, 'types) CND.pt -> bool

(* Is the named declaration a definition? *)
val is_named_defin : ('constr, 'types) CND.pt -> bool

(* --- Deconstructing declarations --- *)

(*
 * Project a component (name, optional value, or type, respectively) of 
 * a rel declaration
 *)
val rel_name : ('constr, 'types) CRD.pt -> Name.t
val rel_value : ('constr, 'types) CRD.pt -> 'constr option
val rel_type : ('constr, 'types) CRD.pt -> 'types

(*
 * Project a component (identifier, optional value, or type, respectively) of 
 * a named declaration
 *)
val named_ident : ('constr, 'types) CND.pt -> Id.t
val named_value : ('constr, 'types) CND.pt -> 'constr option
val named_type : ('constr, 'types) CND.pt -> 'types

(* --- Mapping over contexts --- *)

(*
 * Map over a rel context with environment kept in synch
 *)
val map_rel_context :
  env -> (env -> CRD.t -> 'a) -> Context.Rel.t -> 'a list

(*
 * Map over a named context with environment kept in synch
 *)
val map_named_context :
  env -> (env -> CND.t -> 'a) -> Context.Named.t -> 'a list     

(* --- Binding in contexts --- *)

(*
 * Bind all local declarations in the relative context onto the body term as
 * products, substituting away (i.e., zeta-reducing) any local definitions.
 *)
val smash_prod_assum : Context.Rel.t -> types -> types
val smash_lam_assum : Context.Rel.t -> constr -> constr

(*
 * Decompose the first n product bindings, zeta-reducing let bindings to reveal
 * further product/lambda bindings when necessary.
 *)
val decompose_prod_n_zeta : int -> types -> Context.Rel.t * types
val decompose_lam_n_zeta : int -> constr -> Context.Rel.t * constr

(*
 * Reconstruct local bindings around a term
 *)
val recompose_prod_assum : Context.Rel.t -> types -> types
val recompose_lam_assum : Context.Rel.t -> types -> types

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
  (constr, types) CRD.pt list ->
  (constr, types) CRD.pt list
            
(* --- Getting bindings for certain kinds of terms --- *)

(*
 * Certain terms create bindings you need to push to the environment as you
 * recurse. These functions give you those bindings.
 *)
                                                              
val bindings_for_inductive :
  env -> mutual_inductive_body -> one_inductive_body array -> CRD.t list

val bindings_for_fix :
  name array -> types array -> CRD.t list

(* --- Combining contexts --- *) 

(*
 * Append two contexts (inner first, outer second), shifting internal indices.
 *
 * The input contexts are assumed to share the same environment, such that any
 * external indices inside the now-inner context must be shifted to pass over
 * the now-outer context.
 *)
val context_app : Context.Rel.t -> Context.Rel.t -> Context.Rel.t
