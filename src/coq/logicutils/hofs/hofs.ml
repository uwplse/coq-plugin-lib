(* Higher-order functions on terms *)

open Environ
open Constr
open Contextutils
open Envutils
open Utilities
open Names
open Evd
open Stateutils

(* Predicates to determine whether to apply a mapped function *)
type ('a, 'b) pred = 'a -> 'b -> bool
type 'b unit_pred = 'b -> bool
type ('a, 'b) pred_with_env = env -> evar_map -> 'a -> 'b -> evar_map * bool
type 'b unit_pred_with_env = env -> evar_map -> 'b -> evar_map * bool

(* Functions to use in maps *)
type ('a, 'b) transformer = 'a -> 'b -> 'b
type 'b unit_transformer = 'b -> 'b
type ('a, 'b) list_transformer = 'a -> 'b -> 'b list
type ('a, 'b) transformer_with_env = env -> evar_map -> 'a -> 'b -> evar_map * 'b
type 'b unit_transformer_with_env = env -> evar_map -> 'b -> evar_map * 'b
type ('a, 'b) list_transformer_with_env = env -> evar_map -> 'a -> 'b -> evar_map * 'b list
type ('a, 'b) transformer_with_env_types = env -> evar_map -> 'a -> types -> evar_map * 'b
type ('a, 'b) list_transformer_with_env_types = env -> evar_map -> 'a -> types -> (evar_map * 'b) list

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

(*
 * TODO changing function order so they end with tr and sigma will make this
 * much neater with bind
 *)

(* --- Terms --- *)

(*
 * Map recursively on an array of arguments, threading the state through
 * the result
 *)
let map_rec_args map_rec env sigma a args =
  map_state_array (fun tr sigma -> map_rec env sigma a tr) args sigma

(*
 * Same, but return all combinations of the results
 * TODO can we reuse map_rec_args, or is the type system too weak?
 *)
let map_rec_args_cartesian map_rec env sigma a args =
  bind
    (map_state_array (fun tr sigma -> map_rec env sigma a tr) args)
    (fun l -> ret (combine_cartesian_append l))
    sigma

(*
 * Predicate version
 *)
let exists_args map_rec env sigma a args =
  exists_state (fun tr sigma -> map_rec env sigma a tr) (Array.to_list args) sigma

(*
 * Recurse on a mapping function with an environment for a fixpoint
 *)
let map_rec_env_fix map_rec d env (sigma : evar_map) a ns ts (trm : types) =
  let fix_bindings = bindings_for_fix ns ts in
  let env_fix = push_rel_context fix_bindings env in
  let n = List.length fix_bindings in
  let d_n = List.fold_left (fun a' _ -> d a') a (range 0 n) in
  map_rec env_fix sigma d_n trm

(*
 * Recurse on a mapping function with an environment for a fixpoint
 * TODO do we need both of these, or is type system too weak?
 *)
let map_rec_env_fix_cartesian (map_rec : ('a, 'b) list_transformer_with_env) d env sigma a ns ts =
  let fix_bindings = bindings_for_fix ns ts in
  let env_fix = push_rel_context fix_bindings env in
  let n = List.length fix_bindings in
  let d_n = List.fold_left (fun a' _ -> d a') a (range 0 n) in
  map_rec env_fix sigma d_n

(* 
 * TODO explain
 *)
let map_term_env_rec map_rec f d env sigma a trm =
  match kind trm with
  | Cast (c, k, t) ->
     let sigma, c' = map_rec env sigma a c in
     let sigma, t' = map_rec env sigma a t in
     sigma, mkCast (c', k, t')
  | Prod (n, t, b) ->
     let sigma, t' = map_rec env sigma a t in
     let sigma, b' = map_rec (push_local (n, t) env) sigma (d a) b in
     sigma, mkProd (n, t', b')
  | Lambda (n, t, b) ->
     let sigma, t' = map_rec env sigma a t in
     let sigma, b' = map_rec (push_local (n, t) env) sigma (d a) b in
     sigma, mkLambda (n, t', b')
  | LetIn (n, trm, typ, e) ->
     let sigma, trm' = map_rec env sigma a trm in
     let sigma, typ' = map_rec env sigma a typ in
     let sigma, e' = map_rec (push_let_in (n, e, typ) env) sigma (d a) e in
     sigma, mkLetIn (n, trm', typ', e')
  | App (fu, args) ->
     let sigma, fu' = map_rec env sigma a fu in
     let sigma, args' = map_rec_args map_rec env sigma a args in
     sigma, mkApp (fu', args')
  | Case (ci, ct, m, bs) ->
     let sigma, ct' = map_rec env sigma a ct in
     let sigma, m' = map_rec env sigma a m in
     let sigma, bs' = map_rec_args map_rec env sigma a bs in
     sigma, mkCase (ci, ct', m', bs')
  | Fix ((is, i), (ns, ts, ds)) ->
     let sigma, ts' = map_rec_args map_rec env sigma a ts in
     let sigma, ds' = map_rec_args (fun env sigma a trm -> map_rec_env_fix map_rec d env sigma a ns ts trm) env sigma a ds in (* TODO refactor *)
     sigma, mkFix ((is, i), (ns, ts', ds'))
  | CoFix (i, (ns, ts, ds)) ->
     let sigma, ts' = map_rec_args map_rec env sigma a ts in
     let sigma, ds' = map_rec_args (fun env sigma a trm -> map_rec_env_fix map_rec d env sigma a ns ts trm) env sigma a ds in (* TODO refactor *)
     sigma, mkCoFix (i, (ns, ts', ds'))
  | Proj (p, c) ->
     let sigma, c' = map_rec env sigma a c in
     sigma, mkProj (p, c')
  | _ ->
     f env sigma a trm
  
(*
 * Map a function over a term in an environment
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
let rec map_term_env f d env sigma a trm =
  map_term_env_rec (map_term_env f d) f d env sigma a trm

(*
 * Map a function over a term, when the environment doesn't matter
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
let map_term f d a trm =
  snd
    (map_term_env
       (fun _ _ a t -> Evd.empty, f a t)
       d
       empty_env
       Evd.empty
       a
       trm)

(* 
 * TODO explain
 *)
let map_subterms_env_rec map_rec f d env sigma a trm =
  match kind trm with
  | Cast (c, k, t) ->
     let sigma, cs' = map_rec env sigma a c in
     let sigma, ts' = map_rec env sigma a t in
     sigma, combine_cartesian (fun c' t' -> mkCast (c', k, t')) cs' ts'
  | Prod (n, t, b) ->
     let sigma, ts' = map_rec env sigma a t in
     let sigma, bs' = map_rec (push_local (n, t) env) sigma (d a) b in
     sigma, combine_cartesian (fun t' b' -> mkProd (n, t', b')) ts' bs'
  | Lambda (n, t, b) ->
     let sigma, ts' = map_rec env sigma a t in
     let sigma, bs' = map_rec (push_local (n, t) env) sigma (d a) b in
     sigma, combine_cartesian (fun t' b' -> mkLambda (n, t', b')) ts' bs'
  | LetIn (n, trm, typ, e) ->
     let sigma, trms' = map_rec env sigma a trm in
     let sigma, typs' = map_rec env sigma a typ in
     let sigma, es' = map_rec (push_let_in (n, e, typ) env) sigma (d a) e in
     sigma, combine_cartesian (fun trm' (typ', e') -> mkLetIn (n, trm', typ', e')) trms' (cartesian typs' es')
  | App (fu, args) ->
     let sigma, fus' = map_rec env sigma a fu in
     let sigma, argss' = map_rec_args_cartesian map_rec env sigma a args in
     sigma, combine_cartesian (fun fu' args' -> mkApp (fu', args')) fus' argss'
  | Case (ci, ct, m, bs) ->
     let sigma, cts' = map_rec env sigma a ct in
     let sigma, ms' = map_rec env sigma a m in
     let sigma, bss' = map_rec_args_cartesian map_rec env sigma a bs in
     sigma, combine_cartesian (fun ct' (m', bs') -> mkCase (ci, ct', m', bs')) cts' (cartesian ms' bss')
  | Fix ((is, i), (ns, ts, ds)) ->
     let sigma, tss' = map_rec_args_cartesian map_rec env sigma a ts in
     let sigma, dss' = map_rec_args_cartesian (fun env sigma a trm -> map_rec_env_fix map_rec d env sigma a ns ts trm) env sigma a ds in (* TODO refactor *)
     sigma, combine_cartesian (fun ts' ds' -> mkFix ((is, i), (ns, ts', ds'))) tss' dss'
  | CoFix (i, (ns, ts, ds)) ->
     let sigma, tss' = map_rec_args_cartesian map_rec env sigma a ts in
     let sigma, dss' = map_rec_args_cartesian (fun env sigma a trm -> map_rec_env_fix map_rec d env sigma a ns ts trm) env sigma a ds in (* TODO refactor *)
     sigma, combine_cartesian (fun ts' ds' -> mkCoFix (i, (ns, ts', ds'))) tss' dss'
  | Proj (p, c) ->
     let sigma, cs' = map_rec env sigma a c in
     sigma, List.map (fun c' -> mkProj (p, c')) cs'
  | _ ->
     f env sigma a trm
  
(*
 * Map a function over subterms of a term in an environment
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
let rec map_subterms_env f d env sigma a trm : evar_map * types list =
  map_subterms_env_rec (map_subterms_env f d) f d env sigma a trm

(*
 * Map a function over subterms of a term, when the environment doesn't matter
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
let map_subterms f d a trm : types list =
  snd
    (map_subterms_env
       (fun _ _ a t -> Evd.empty, f a t)
       d
       empty_env
       Evd.empty
       a
       trm)

(*
 * Map a function over a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
let rec map_term_env_if p f d env sigma a trm =
  branch_state
    (fun trm sigma -> p env sigma a trm)
    (fun trm sigma -> f env sigma a trm)
    (fun trm sigma ->
      map_term_env_rec
        (map_term_env_if p f d)
        (fun _ sigma _ tr -> sigma, tr)
        d
        env
        sigma
        a
        trm)
    trm
    sigma

(* 
 * TODO explain
 *)
let map_term_env_rec_shallow map_rec f d env sigma a trm =
  match kind trm with
  | Cast (c, k, t) ->
     let sigma, c' = map_rec env sigma a c in
     let sigma, t' = map_rec env sigma a t in
     sigma, mkCast (c', k, t')
  | Prod (n, t, b) ->
     let sigma, t' = map_rec env sigma a t in
     let sigma, b' = map_rec (push_local (n, t) env) sigma (d a) b in
     sigma, mkProd (n, t', b')
  | Lambda (n, t, b) ->
     let sigma, t' = map_rec env sigma a t in
     let sigma, b' = map_rec (push_local (n, t) env) sigma (d a) b in
     sigma, mkLambda (n, t', b')
  | LetIn (n, trm, typ, e) ->
     let sigma, trm' = map_rec env sigma a trm in
     let sigma, typ' = map_rec env sigma a typ in
     let sigma, e' = map_rec (push_let_in (n, e, typ) env) sigma (d a) e in
     sigma, mkLetIn (n, trm', typ', e')
  | App (fu, args) ->
     let sigma, fu' = map_rec env sigma a fu in
     let sigma, args' =
       let map_rec_shallow env sigma a t =
         if isLambda t then sigma, t else map_rec env sigma a t
       in map_rec_args map_rec_shallow env sigma a args
     in sigma, mkApp (fu', args')
  | Case (ci, ct, m, bs) ->
     let sigma, ct' = map_rec env sigma a ct in
     let sigma, m' = map_rec env sigma a m in
     let sigma, bs' = map_rec_args map_rec env sigma a bs in
     sigma, mkCase (ci, ct', m', bs')
  | Fix ((is, i), (ns, ts, ds)) ->
     let sigma, ts' = map_rec_args map_rec env sigma a ts in
     let sigma, ds' = map_rec_args (fun env sigma a trm -> map_rec_env_fix map_rec d env sigma a ns ts trm) env sigma a ds in (* TODO refactor *)
     sigma, mkFix ((is, i), (ns, ts', ds'))
  | CoFix (i, (ns, ts, ds)) ->
     let sigma, ts' = map_rec_args map_rec env sigma a ts in
     let sigma, ds' = map_rec_args (fun env sigma a trm -> map_rec_env_fix map_rec d env sigma a ns ts trm) env sigma a ds in (* TODO refactor *)
     sigma, mkCoFix (i, (ns, ts', ds'))
  | Proj (p, c) ->
     let sigma, c' = map_rec env sigma a c in
     sigma, mkProj (p, c')
  | _ ->
     f env sigma a trm

(*
 * Map a function over a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Don't recurse into lambda arguments
 * Return a new term
 *)
let rec map_term_env_if_shallow p f d env sigma a trm =
  branch_state
    (fun trm sigma -> p env sigma a trm)
    (fun trm sigma -> f env sigma a trm)
    (fun trm sigma ->
      map_term_env_rec_shallow
        (map_term_env_if_shallow p f d)
        (fun _ sigma _ t -> sigma, t)
        d
        env
        sigma
        a
        trm)
    trm
    sigma

(*
 * Lazy version of map_term_env_if
 *)
let rec map_term_env_if_lazy p f d env sigma a trm =
  bind
    (fun sigma ->
      map_term_env_rec
        (map_term_env_if_lazy p f d)
        (fun _ sigma _ t -> sigma, t)
        d
        env
        sigma
        a
        trm)
    (branch_state
       (fun trm sigma -> p env sigma a trm)
       (fun trm sigma -> f env sigma a trm)
       ret)
    sigma

(*
 * Map a function over a term where the environment doesn't matter
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
let map_term_if p f d a trm : types =
  snd
    (map_term_env_if
       (fun _ _ a t -> Evd.empty, p a t)
       (fun _ _ a t -> Evd.empty, f a t)
       d
       empty_env
       Evd.empty
       a
       trm)

(* Lazy version *)
let map_term_if_lazy p f d a trm =
  snd
    (map_term_env_if_lazy
       (fun _ _ a t -> Evd.empty, p a t)
       (fun _ _ a t -> Evd.empty, f a t)
       d
       empty_env
       Evd.empty
       a
       trm)
                  
(*
 * Map a function over subterms of a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
let rec map_subterms_env_if p f d env sigma a trm =
  branch_state
    (fun trm sigma -> p env sigma a trm)
    (fun trm sigma -> f env sigma a trm)
    (fun trm sigma ->
      map_subterms_env_rec
        (map_subterms_env_if p f d)
        (fun _ sigma _ trm -> sigma, [trm])
        d
        env
        sigma
        a
        trm)
    trm
    sigma

(*
 * Map a function over subterms of a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly, but always recurse
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
let rec map_subterms_env_if_combs p f d env sigma a trm =
  bind
    (branch_state
      (fun trm sigma -> p env sigma a trm)
      (fun trm sigma -> f env sigma a trm)
      (fun trm -> ret [trm])
      trm)
    (flat_map_state
       (fun trm sigma ->
         map_subterms_env_rec
           (map_subterms_env_if_combs p f d)
           (fun _ sigma _ trm -> sigma, [trm])
           d
           env
           sigma
           a
           trm))
    sigma
                                                  
(*
 * Like map_term_env_if, but make a list of subterm results
 *)
let rec map_term_env_if_list p f d env sigma a trm =
  let map_rec = map_term_env_if_list p f d in
  let sigma_t, p_holds = p env sigma a trm in
  if p_holds then
    [f env sigma_t a trm]
  else
    match kind trm with
    | Cast (c, k, t) ->
       let c' = map_rec env sigma a c in
       let t' = map_rec env sigma a t in
       List.append c' t'
    | Prod (n, t, b) ->
       let t' = map_rec env sigma a t in
       let b' = map_rec (push_local (n, t) env) sigma (d a) b in
       List.append t' b'
    | Lambda (n, t, b) ->
       let t' = map_rec env sigma a t in
       let b' = map_rec (push_local (n, t) env) sigma (d a) b in
       List.append t' b'
    | LetIn (n, trm, typ, e) ->
       let trm' = map_rec env sigma a trm in
       let typ' = map_rec env sigma a typ in
       let e' = map_rec (push_let_in (n, e, typ) env) sigma (d a) e in
       List.append trm' (List.append typ' e')
    | App (fu, args) ->
       let fu' = map_rec env sigma a fu in
       let args' = Array.map (map_rec env sigma a) args in
       List.append fu' (List.flatten (Array.to_list args'))
    | Case (ci, ct, m, bs) ->
       let ct' = map_rec env sigma a ct in
       let m' = map_rec env sigma a m in
       let bs' = Array.map (map_rec env sigma a) bs in
       List.append ct' (List.append m' (List.flatten (Array.to_list bs')))
    | Fix ((is, i), (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env sigma a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env sigma a ns ts) ds in
       List.append (List.flatten (Array.to_list ts')) (List.flatten (Array.to_list ds'))
    | CoFix (i, (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env sigma a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env sigma a ns ts) ds in
       List.append (List.flatten (Array.to_list ts')) (List.flatten (Array.to_list ds'))
    | Proj (pr, c) ->
       map_rec env sigma a c
    | _ ->
       []

(*
 * Map a function over subterms of a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function after recursing
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *
 * TODO redundant calls right now
 * TODO rename... confusing different subterm combination stuff in here
 *)
let rec map_subterms_env_if_lazy p f d env sigma a trm =
  bind
    (fun sigma ->
      map_subterms_env_rec
        (map_subterms_env_if_lazy p f d)
        (fun _ sigma _ trm -> sigma, [trm])
        d
        env
        sigma
        a
        trm)
    (flat_map_state
       (branch_state
          (fun trm sigma -> p env sigma a trm)
          (fun trm sigma -> f env sigma a trm)
          (fun trm -> ret [trm])))
    sigma

(* --- Propositions --- *)

(*
 * Like map_term_env_if, but just return true if the proposition is satisfied,
 * and false otherwise.
 *
 * We can make this even more general and just take a combinator
 * and a mapping function and so on, in the future.
 *)
let rec exists_subterm_env p d env sigma (a : 'a) (trm : types) : evar_map * bool =
  let map_rec = exists_subterm_env p d in
  branch_state
    (fun trm sigma -> p env sigma a trm)
    (fun _ -> ret true)
    (fun trm sigma ->
      match kind trm with
      | Cast (c, k, t) ->
         let sigma, c' = map_rec env sigma a c in
         let sigma, t' = map_rec env sigma a t in
         sigma, c' || t'
      | Prod (n, t, b) ->
         let sigma, t' = map_rec env sigma a t in
         let sigma, b' = map_rec (push_local (n, t) env) sigma (d a) b in
         sigma, t' || b'
      | Lambda (n, t, b) ->
         let sigma, t' = map_rec env sigma a t in
         let sigma, b' = map_rec (push_local (n, t) env) sigma (d a) b in
         sigma, t' || b'
      | LetIn (n, trm, typ, e) ->
         let sigma, trm' = map_rec env sigma a trm in
         let sigma, typ' = map_rec env sigma a typ in
         let sigma, e' = map_rec (push_let_in (n, e, typ) env) sigma (d a) e in
         sigma, trm' || typ' || e'
      | App (fu, args) ->
         let sigma, fu' = map_rec env sigma a fu in
         let sigma, args' = exists_args map_rec env sigma a args in
         sigma, fu' || args'
      | Case (ci, ct, m, bs) ->
         let sigma, ct' = map_rec env sigma a ct in
         let sigma, m' = map_rec env sigma a m in
         let sigma, bs' = exists_args map_rec env sigma a bs in
         sigma, ct' || m' || bs'
      | Fix ((is, i), (ns, ts, ds)) ->
         let sigma, ts' = exists_args map_rec env sigma a ts in
         let sigma, ds' = exists_args map_rec env sigma a ds in
         sigma, ts' || ds'
      | CoFix (i, (ns, ts, ds)) ->
         let sigma, ts' = exists_args map_rec env sigma a ts in
         let sigma, ds' = exists_args map_rec env sigma a ds in
         sigma, ts' || ds'
      | Proj (pr, c) ->
         map_rec env sigma a c
      | _ ->
         sigma, false)
    trm
    sigma
                  
(* exists_subterm_env with an empty environment *)
let exists_subterm p d a t =
  snd
    (exists_subterm_env
       (fun _ _ a t -> Evd.empty, p a t)
       d
       empty_env
       Evd.empty
       a
       t)

(* all constant subterms that match a stateless predicate *)
let all_const_subterms p d a t =
  List.map
    snd
    (List.map
       snd
       (map_term_env_if_list
          (fun _ sigma a t -> sigma, isConst t && p a t)
          (fun en sigma _ t -> sigma, (en, t))
          d
          empty_env
          Evd.empty
          a
          t))
              
(* --- Variations --- *)

(* map env without any a *)
let map_unit_env mapper p f env sigma trm =
  mapper
    (fun en sigma _ t -> p en sigma t)
    (fun en sigma _ t -> f en sigma t)
    (fun _ -> ())
    env
    sigma
    ()
    trm
         
(* map without any a *)
let map_unit mapper p f trm =
  mapper (fun _ t -> p t) (fun _ t -> f t) (fun _ -> ()) () trm

(* Some simple combinations *)
let map_unit_env_if = map_unit_env map_term_env_if
let map_unit_env_if_lazy = map_unit_env map_term_env_if_lazy
let map_unit_if = map_unit map_term_if
let map_unit_if_lazy = map_unit map_term_if_lazy
