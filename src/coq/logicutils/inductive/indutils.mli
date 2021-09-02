(*
 * Inductive types and their eliminators (TODO organize, maybe move some stuff)
 *)

open Environ
open Constr
open Evd
open Names
open Declarations

       
(*
 * Fail if the inductive type is mutually inductive or coinductive
 * TODO move
 *)
val check_inductive_supported : mutual_inductive_body -> unit

(*
 * Get the number of constructors for an inductive type
 *)
val num_constrs : mutual_inductive_body -> int

(*
 * Determine if a term represents an inductive eliminator
 * For now, this is a naive syntactic check
 *)
val is_elim : env -> types -> bool

(*
 * Get an inductive type from an eliminator, if possible
 *)
val inductive_of_elim : env -> pconstant -> MutInd.t option

(*
 * Lookup the eliminator over the type sort
 *)
val type_eliminator : env -> inductive -> types

(*
 * Applications of eliminators
 *)
type elim_app =
  {
    elim : types;
    pms : types list;
    p : types;
    cs : types list;
    final_args : types list;
  }

val apply_eliminator : elim_app -> types
val deconstruct_eliminator : env-> evar_map -> types -> evar_map * elim_app

(*
 * Given the recursive type and the type of a case of an eliminator,
 * determine the number of inductive hypotheses
 *)
val num_ihs : env -> evar_map -> types -> types -> int

(* Determine whether template polymorphism is used for a one_inductive_body *)
val is_ind_body_template : one_inductive_body -> bool

(* Construct the arity of an inductive type from a one_inductive_body *)
val arity_of_ind_body : one_inductive_body -> types

(*
 * For an inductive type in an environment, return the inductive's arity and
 * recursion-quantified constructor types, all consistently instantiated with fresh
 * universe levels, and return the universe-synchronized environment. If global
 * is true, the global environment is also synchronized with the new universe
 * levels and constraints. A descriptor for the inductive type's universe
 * properties is also returned.
 *)
val open_inductive : ?global:bool -> env -> Inductive.mind_specif -> env * Entries.inductive_universes * types * types list

(*
 * Declare a new inductive type in the global environment. Note that the arity
 * must quantify all parameters and that each constructor type must quantify
 * a recursive reference and then all parameters (i.e.,
 * forall (I : arity) (P : params), ...).
 *)
val declare_inductive : Id.t -> Id.t list -> bool -> Entries.inductive_universes -> int -> types -> types list -> inductive

