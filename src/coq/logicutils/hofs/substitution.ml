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
    (fun (s, d) -> (shift env s, shift env d))
    env
    sigma
    (src, dst)
    trm

(* Map all combinations of a substitution over a term *)
let all_substs_combs p env sigma (src, dst) trm : evar_map * types list =
  map_subterms_env_if
    (fun env sigma (s, _) t -> p env sigma s t)
    (fun _ sigma (_, d) t -> sigma, [d; t])
    (fun (s, d) -> (shift env s, shift env d))
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

(* Same, but equal *)
let all_eq_substs env (src, dst) trm =
  snd
    (all_substs
       (fun _ _ t1 t2 -> Evd.empty, equal t1 t2)
       env
       (Evd.from_env env)
       (src, dst)
       trm)

(*
 * Check if a subterm matches applies a constructor function pat to
 * an argument with the type of itself
 *)
let constructs_recursively env sigma c trm : evar_map * bool =
  if isApp trm then
    try
      let (f, args) = destApp trm in
      branch_state
        (fun t sigma -> convertible env sigma f t)
        (fun t ->
          exists_state
            (fun t sigma -> types_convertible env sigma trm t)
            (Array.to_list args))
        (fun _ -> ret false)
        c
        sigma
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
      let (_, args_t) = destApp t in
      find_state
        (fun trm sigma -> types_convertible env sigma t trm)
        (Array.to_list args_t)
        sigma)
    (shift env)
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

type global_substitution = GlobRef.t Names.GlobRef.Map.t

(* Substitute global references throughout a term *)
let rec subst_globals env subst (term : constr) =
  map_term_if env
    (fun _ t -> isConst t || isInd t || isConstruct t || isVar t || isCase t)
    (fun _ t ->
      try
        pglobal_of_constr t |>
        map_puniverses (flip Names.GlobRef.Map.find subst) |>
        constr_of_pglobal
      with _ ->
        match kind t with
        | Case (ci, u, pms, p, iv, c, brs) ->
           let (ci, p, iv, c, brs) = Inductive.expand_case (Global.env ()) (ci, u, pms, p, iv, c, brs) in
           let ci_ind' = destInd (subst_globals env subst (mkInd ci.ci_ind)) in
           let ci' = { ci with ci_ind = fst ci_ind' } in 
           let b' = subst_globals env subst c in
           let p' = subst_globals env subst p in
           let brs' = Array.map (subst_globals env subst) brs in
           mkCase (Inductive.contract_case (Global.env ()) (ci', p', iv, b', brs'))               
        | _ -> t)
    (fun _ -> ())
    ()
    term
