(* Substitution auxiliary functions *)

open Environ
open Evd
open Constr
open Hofs
open Debruijn
open Convertibility
open Defutils
open Names
open Utilities

(* TODO clean up so retrieval is easier *)
type ('a, 'b) substitution = env -> evar_map -> 'a -> types -> 'b
type 'a comb_substitution = ('a, types list) substitution
type 'a type_substitution = ('a, types) substitution

(* 
 * Map a substitution over a term
 *)
let all_substs p env sigma (src, dst) trm : types =
  snd
    (map_term_env_if
       (fun env sigma (s, _) t -> p env sigma s t)
       (fun _ sigma (_, d) _ -> sigma, d)
       (fun (s, d) -> (shift s, shift d))
       env
       sigma
       (src, dst)
       trm)

(* Map all combinations of a substitution over a term *)
let all_substs_combs p env sigma (src, dst) trm : types list =
  snd
    (map_subterms_env_if
       (fun env sigma (s, _) t -> p env sigma s t)
       (fun _ sigma (_, d) t -> sigma, [d; t])
       (fun (s, d) -> (shift s, shift d))
       env
       sigma
       (src, dst)
       trm)

(* In env, substitute all subterms of trm that are convertible to src with dst
   TODO do we want to thread the evar_map through for the conv ones
   and check the result? Does that gain us anything? How does it
   impact performance? *)
let all_conv_substs : (types * types) type_substitution =
  all_substs convertible

(* In env, substitute all subterms of trm that have a convertible type to the type of src with dst *)
let all_typ_substs : (types * types) type_substitution =
  all_substs types_convertible

(* Same, but equal *)
let all_eq_substs =
  all_substs (fun _ _ -> equal) empty_env Evd.empty

(*
 * Check if a subterm matches applies a constructor function pat to
 * an argument with the type of itself
 *)
let constructs_recursively env sigma c trm : bool =
  if isApp trm then
    try
      let (f, args) = destApp trm in
      let conv = convertible env sigma in
      let types_conv = types_convertible env sigma in
      conv f c && List.exists (types_conv trm) (Array.to_list args)
    with _ ->
      false
  else
    false

(*
 * Map a constructor substitution over a term
 * The constructor is a function c
 * This finds the outermost applications of c to an argument
 * with the type of the term itself, "undoing" the constructor
 * It substitutes in the first argument with that type
 *
 * Can generalize this further
 *)
let all_constr_substs env sigma c trm : types =
  snd
    (map_term_env_if
       constructs_recursively
       (fun env sigma _ t ->
         let (_, args_t) = destApp t in
         sigma, List.find (types_convertible env sigma t) (Array.to_list args_t))
       shift
       env
       sigma
       c
       trm)

(* In env, return all substitutions of subterms of trm that are convertible to src with dst *)
let all_conv_substs_combs : (types * types) comb_substitution =
  all_substs_combs convertible

(* In env, return all substitutions of subterms of trm that have a convertible type to the type of src with dst *)
let all_typ_substs_combs : (types * types) comb_substitution =
  all_substs_combs types_convertible

 (* --- Substituting global references --- *)

type global_substitution = global_reference Globnames.Refmap.t

(* Substitute global references throughout a term *)
let subst_globals subst term =
  let rec aux term =
    try
      pglobal_of_constr term |>
      map_puniverses (flip Globnames.Refmap.find subst) |>
      constr_of_pglobal
    with Not_found ->
      Constr.map aux term
  in
  aux term
