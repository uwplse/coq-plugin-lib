(* Higher-order functions on terms *)

open Environ
open Constr
open Coqterms

(* Predicates to determine whether to apply a mapped function *)
type ('a, 'b) pred = 'a -> 'b -> bool
type 'b unit_pred = 'b -> bool
type ('a, 'b) pred_with_env = env -> ('a, 'b) pred
type 'b unit_pred_with_env = env -> 'b unit_pred

(* Functions to use in maps *)
type ('a, 'b) transformer = 'a -> 'b -> 'b
type 'b unit_transformer = 'b -> 'b
type ('a, 'b) list_transformer = 'a -> 'b -> 'b list
type ('a, 'b) transformer_with_env = env -> 'a -> 'b -> 'b
type 'b unit_transformer_with_env = env -> 'b -> 'b
type ('a, 'b) list_transformer_with_env = env -> 'a -> 'b -> 'b list

(* Updating arguments *)
type 'a updater = 'a -> 'a

(* Mapper functions *)
type ('a, 'b) mapper_with_env =
  ('a, 'b) transformer_with_env ->
  'a updater ->
  ('a, 'b) transformer_with_env

type ('a, 'b) mapper =
  ('a, 'b) transformer ->
  'a updater ->
  ('a, 'b) transformer

type ('a, 'b) list_mapper_with_env =
  ('a, 'b) list_transformer_with_env ->
  'a updater ->
  ('a, 'b) list_transformer_with_env

type ('a, 'b) list_mapper =
  ('a, 'b) list_transformer ->
  'a updater ->
  ('a, 'b) list_transformer

type ('a, 'b) conditional_mapper_with_env =
  ('a, 'b) pred_with_env ->
  ('a, 'b) transformer_with_env ->
  'a updater ->
  ('a, 'b) transformer_with_env

type 'b conditional_unit_mapper_with_env =
  'b unit_pred_with_env ->
  'b unit_transformer_with_env ->
  'b unit_transformer_with_env

type ('a, 'b) conditional_mapper =
  ('a, 'b) pred ->
  ('a, 'b) transformer ->
  'a updater ->
  ('a, 'b) transformer

type 'b unit_conditional_mapper =
  'b unit_pred ->
  'b unit_transformer ->
  'b unit_transformer

type ('a, 'b) conditional_list_mapper_with_env =
  ('a, 'b) pred_with_env ->
  ('a, 'b) list_transformer_with_env ->
  'a updater ->
  ('a, 'b) list_transformer_with_env

type ('a, 'b) proposition_mapper =
  ('a, 'b) pred ->
  'a updater ->
  ('a, 'b) pred
           
type ('a, 'b) proposition_mapper_with_env =
  ('a, 'b) pred_with_env ->
  'a updater ->
  ('a, 'b) pred_with_env

type ('a, 'b) proposition_list_mapper =
  ('a, 'b) pred ->
  'a updater ->
  ('a, 'b) list_transformer

(* --- Terms --- *)

(*
 * The most basic versions of these (map_term and map_term_env) are in
 * coqterms.ml. These are more advanced variations.
 *)

(*
 * Map a function over subterms of a term in an environment
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
val map_subterms_env : ('a, types) list_mapper_with_env

(*
 * Map a function over subterms of a term, when the environment doesn't matter
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
val map_subterms : ('a, types) list_mapper

(*
 * Map a function over a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
val map_term_env_if : ('a, types) conditional_mapper_with_env

(*
 * Like map_term_env_if, but use unit for 'a
 *)
val map_unit_env_if : types conditional_unit_mapper_with_env

(*
 * Like map_term_env_if, but lazy
 *)
val map_term_env_if_lazy : ('a, types) conditional_mapper_with_env

(*
 * Like map_term_env_if_lazy, but use unit for 'a
 *)
val map_unit_env_if_lazy : types conditional_unit_mapper_with_env               
                                  
(*
 * Like map_term_env_if, but don't recurse into lambda arguments
 *)
val map_term_env_if_shallow : ('a, types) conditional_mapper_with_env

(*
 * Like map_term_env_if, but in the empty environment
 *)
val map_term_if : ('a, types) conditional_mapper

(*
 * Like map_term_if, but use unit for 'a
 *)
val map_unit_if : types unit_conditional_mapper

(*
 * Like map_term_env_if_lazy, but use the empty environment
 *)
val map_term_if_lazy : ('a, types) conditional_mapper
    
(*
 * Like map_term_if_lazy, but use unit for 'a
 *)
val map_unit_if_lazy : types unit_conditional_mapper

(*
 * Map a function over subterms of a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
val map_subterms_env_if : ('a, types) conditional_list_mapper_with_env

(*
 * Map a function over subterms of a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly, but always recurse
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
val map_subterms_env_if_combs : ('a, types) conditional_list_mapper_with_env

(*
 * Map a function over subterms of a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function after recursing
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
val map_subterms_env_if_lazy : ('a, types) conditional_list_mapper_with_env

(* --- Propositions --- *)

(*
 * Check if a proposition is ever true over some subterm of a term
 * Return true immediately, if it is
 * In other words, return true if and only if map_term_env_if would
 * apply the function f
 *)
val exists_subterm_env : ('a, types) proposition_mapper_with_env

(* 
 * Like exists_subterm_env, but use the empty environment 
 *)
val exists_subterm : ('a, types) proposition_mapper

(* 
 * Like exists_subterm, but return a list of constant subterms that match
 *)
val all_const_subterms : ('a, types) proposition_list_mapper

(* --- Containment --- *)

(* 
 * Check whether the first term contains the second as a subterm, 
 * using exact syntactic equality
 *)
val contains_term : types -> types -> bool
