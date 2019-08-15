(* Substitution auxiliary functions *)

open Environ
open Evd
open EConstr
open Hofs
open Debruijn
open Convertibility
open Defutils
open Names
open Utilities
open Stateutils

(* TODO clean up so retrieval is easier *)
type ('a, 'b) substitution = env -> evar_map -> 'a -> types -> evar_map * 'b
type 'a comb_substitution = ('a, types list) substitution
type 'a type_substitution = ('a, types) substitution

(* 
 * Map a substitution over a term
 *)
let all_substs p env sigma (src, dst) trm : evar_map * types =
  map_term_env_if
    (fun env sigma (s, _) t -> p env sigma s t)
    (fun _ sigma (_, d) _ -> sigma, d)
    (fun (s, d) -> (shift sigma s, shift sigma d))
    env
    sigma
    (src, dst)
    trm

(* Map all combinations of a substitution over a term *)
let all_substs_combs p env sigma (src, dst) trm : evar_map * types list =
  map_subterms_env_if
    (fun env sigma (s, _) t -> p env sigma s t)
    (fun _ sigma (_, d) t -> sigma, [d; t])
    (fun (s, d) -> (shift sigma s, shift sigma d))
    env
    sigma
    (src, dst)
    trm

(* In env, substitute all subterms of trm that are convertible to src with dst
   TODO do we want to thread the evar_map through for the conv ones
   and check the result? Does that gain us anything? How does it
   impact performance? *)
let all_conv_substs : (types * types) type_substitution =
  all_substs convertible

(* In env, substitute all subterms of trm that have a convertible type to the type of src with dst *)
let all_typ_substs : (types * types) type_substitution =
  all_substs types_convertible

(* Same, but equal (TODO is this safe?) *)
let all_eq_substs (src, dst) trm =
  snd
    (all_substs
       (fun _ _ t1 t2 -> Evd.empty, eq_constr Evd.empty t1 t2)
       empty_env
       Evd.empty
       (src, dst)
       trm)

(*
 * Check if a subterm matches applies a constructor function pat to
 * an argument with the type of itself
 *)
let constructs_recursively env sigma c trm : evar_map * bool =
  if isApp sigma trm then
    try
      let (f, args) = destApp sigma trm in
      let sigma, conv = convertible env sigma f c in
      if conv then
        let types_conv sigma = types_convertible env sigma trm in
        exists_state sigma types_conv (Array.to_list args)
      else
        sigma, conv
    with _ ->
      sigma, false
  else
    sigma, false

(*
 * Map a constructor substitution over a term
 * The constructor is a function c
 * This finds the outermost applications of c to an argument
 * with the type of the term itself, "undoing" the constructor
 * It substitutes in the first argument with that type
 *
 * Can generalize this further
 *)
let all_constr_substs env sigma c trm : evar_map * types =
  map_term_env_if
    constructs_recursively
    (fun env sigma _ t ->
      let (_, args_t) = destApp sigma t in
      find_state
        sigma
        (fun sigma -> types_convertible env sigma t)
        (Array.to_list args_t))
    (shift sigma)
    env
    sigma
    c
    trm

(* In env, return all substitutions of subterms of trm that are convertible to src with dst *)
let all_conv_substs_combs : (types * types) comb_substitution =
  all_substs_combs convertible

(* In env, return all substitutions of subterms of trm that have a convertible type to the type of src with dst *)
let all_typ_substs_combs : (types * types) comb_substitution =
  all_substs_combs types_convertible

 (* --- Substituting global references --- *)

type global_substitution = global_reference Globnames.Refmap.t

(*
 * From Coq standard library, but on EConstr
 *)
let map_puniverses f (x, u) = (f x, u)

(* Substitute global references throughout a term *)
let subst_globals sigma subst term =
  let rec aux term =
    try
      pglobal_of_constr sigma term |>
      map_puniverses (flip Globnames.Refmap.find subst) |>
      constr_of_pglobal
    with Not_found ->
      EConstr.map sigma aux term
  in aux term
