(*
 * Term and type transformation (has side effects)
 * TODO explain more, maybe move later
 *)

open Environ
open Constr
open Evd
open Names
open Declarations
open Substitution

(* Type-sensitive transformation of terms *)
type constr_transformer = env -> evar_map -> constr -> evar_map * constr

(*
 * Declare a new constant under the given name with the transformed term and
 * type from the given constant.
 *
 * NOTE: Global side effects.
 *)
val transform_constant :
  Id.t -> constr_transformer -> Opaqueproof.opaque constant_body -> evar_map * Constant.t

(*
 * Declare a new inductive family under the given name with the transformed type
 * arity and constructor types from the given inductive definition. Names for
 * the constructors remain the same.
 *
 * NOTE: Global side effects.
 *)
val transform_inductive :
  Id.t -> constr_transformer -> mutual_inductive_body * one_inductive_body -> evar_map * inductive

(*
 * Declare a new module structure under the given name with the compositionally
 * transformed (i.e., forward-substituted) components from the given module
 * structure. Names for the components remain the same.
 *
 * The optional initialization function is called immediately after the module
 * structure begins, and its returned subsitution is applied to all other module
 * elements.
 *
 * NOTE: Does not support functors or nested modules.
 * NOTE: Global side effects.
 *)
val transform_module_structure :
  env -> ?init:(unit -> global_substitution) -> ?opaques:(Names.GlobRef.Set.t) -> Id.t -> constr_transformer -> module_body -> ModPath.t
