(* Higher-order functions on terms *)

open Environ
open Constr
open Coqterms
open Utilities
open Names
open Debruijn

module CRD = Context.Rel.Declaration
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
 * Recurse on a mapping function with an environment for a fixpoint
 *)
let map_rec_env_fix_cartesian (map_rec : ('a, 'b) list_transformer_with_env) (d : 'a updater) (env : env) (a : 'a) (ns : Name.t array) (ts : types array) =
  let fix_bindings = bindings_for_fix ns ts in
  let env_fix = push_rel_context fix_bindings env in
  let n = List.length fix_bindings in
  let d_n = List.fold_left (fun a' _ -> d a') a (range 0 n) in
  map_rec env_fix d_n

(*
 * Map a function over subterms of a term in an environment
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
let rec map_subterms_env f d env a trm : types list =
  let map_rec = map_subterms_env f d in
  match kind trm with
  | Cast (c, k, t) ->
     let cs' = map_rec env a c in
     let ts' = map_rec env a t in
     combine_cartesian (fun c' t' -> mkCast (c', k, t')) cs' ts'
  | Prod (n, t, b) ->
     let ts' = map_rec env a t in
     let bs' = map_rec (push_rel CRD.(LocalAssum(n, t)) env) (d a) b in
     combine_cartesian (fun t' b' -> mkProd (n, t', b')) ts' bs'
  | Lambda (n, t, b) ->
     let ts' = map_rec env a t in
     let bs' = map_rec (push_rel CRD.(LocalAssum(n, t)) env) (d a) b in
     combine_cartesian (fun t' b' -> mkLambda (n, t', b')) ts' bs'
  | LetIn (n, trm, typ, e) ->
     let trms' = map_rec env a trm in
     let typs' = map_rec env a typ in
     let es' = map_rec (push_rel CRD.(LocalDef(n, e, typ)) env) (d a) e in
     combine_cartesian (fun trm' (typ', e') -> mkLetIn (n, trm', typ', e')) trms' (cartesian typs' es')
  | App (fu, args) ->
     let fus' = map_rec env a fu in
     let argss' = combine_cartesian_append (Array.map (map_rec env a) args) in
     combine_cartesian (fun fu' args' -> mkApp (fu', args')) fus' argss'
  | Case (ci, ct, m, bs) ->
     let cts' = map_rec env a ct in
     let ms' = map_rec env a m in
     let bss' = combine_cartesian_append (Array.map (map_rec env a) bs) in
     combine_cartesian (fun ct' (m', bs') -> mkCase (ci, ct', m', bs')) cts' (cartesian ms' bss')
  | Fix ((is, i), (ns, ts, ds)) ->
     let tss' = combine_cartesian_append (Array.map (map_rec env a) ts) in
     let dss' = combine_cartesian_append (Array.map (map_rec_env_fix_cartesian map_rec d env a ns ts) ds) in
     combine_cartesian (fun ts' ds' -> mkFix ((is, i), (ns, ts', ds'))) tss' dss'
  | CoFix (i, (ns, ts, ds)) ->
     let tss' = combine_cartesian_append (Array.map (map_rec env a) ts) in
     let dss' = combine_cartesian_append (Array.map (map_rec_env_fix_cartesian map_rec d env a ns ts) ds) in
     combine_cartesian (fun ts' ds' -> mkCoFix (i, (ns, ts', ds'))) tss' dss'
  | Proj (p, c) ->
     let cs' = map_rec env a c in
     List.map (fun c' -> mkProj (p, c')) cs'
  | _ ->
     f env a trm

(*
 * Map a function over subterms of a term, when the environment doesn't matter
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
let map_subterms f d a trm : types list =
  map_subterms_env (fun _ a t -> f a t) d empty_env a trm

(*
 * Map a function over a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
let rec map_term_env_if p f d (env : env) (a : 'a) (trm : types) : types =
  let map_rec = map_term_env_if p f d in
  if p env a trm then
    f env a trm
  else
    match kind trm with
    | Cast (c, k, t) ->
       let c' = map_rec env a c in
       let t' = map_rec env a t in
       mkCast (c', k, t')
    | Prod (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_local (n, t') env) (d a) b in
       mkProd (n, t', b')
    | Lambda (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_local (n, t') env) (d a) b in
       mkLambda (n, t', b')
    | LetIn (n, trm, typ, e) ->
       let trm' = map_rec env a trm in
       let typ' = map_rec env a typ in
       let e' = map_rec (push_let_in (n, e, typ') env) (d a) e in
       mkLetIn (n, trm', typ', e')
    | App (fu, args) ->
       let fu' = map_rec env a fu in
       let args' = Array.map (map_rec env a) args in
       mkApp (fu', args')
    | Case (ci, ct, m, bs) ->
       let ct' = map_rec env a ct in
       let m' = map_rec env a m in
       let bs' = Array.map (map_rec env a) bs in
       mkCase (ci, ct', m', bs')
    | Fix ((is, i), (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
       mkFix ((is, i), (ns, ts', ds'))
    | CoFix (i, (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
       mkCoFix (i, (ns, ts', ds'))
    | Proj (pr, c) ->
       let c' = map_rec env a c in
       mkProj (pr, c')
    | _ ->
       trm

(*
 * Map a function over a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Don't recurse into lambda arguments
 * Return a new term
 *)
let rec map_term_env_if_shallow p f d env a trm : types =
  let map_rec = map_term_env_if_shallow p f d in
  if p env a trm then
    f env a trm
  else
    match kind trm with
    | Cast (c, k, t) ->
       let c' = map_rec env a c in
       let t' = map_rec env a t in
       mkCast (c', k, t')
    | Prod (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_rel CRD.(LocalAssum(n, t')) env) (d a) b in
       mkProd (n, t', b')
    | Lambda (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_rel CRD.(LocalAssum(n, t')) env) (d a) b in
       mkLambda (n, t', b')
    | LetIn (n, trm, typ, e) ->
       let trm' = map_rec env a trm in
       let typ' = map_rec env a typ in
       let e' = map_rec (push_rel CRD.(LocalDef(n, e, typ')) env) (d a) e in
       mkLetIn (n, trm', typ', e')
    | App (fu, args) ->
       let fu' = map_rec env a fu in
       let args' =
         Array.map
           (fun t -> if isLambda t then t else map_rec env a t)
           args
       in mkApp (fu', args')
    | Case (ci, ct, m, bs) ->
       let ct' = map_rec env a ct in
       let m' = map_rec env a m in
       let bs' = Array.map (map_rec env a) bs in
       mkCase (ci, ct', m', bs')
    | Fix ((is, i), (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
       mkFix ((is, i), (ns, ts', ds'))
    | CoFix (i, (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
       mkCoFix (i, (ns, ts', ds'))
    | Proj (pr, c) ->
       let c' = map_rec env a c in
       mkProj (pr, c')
    | _ ->
       trm

(*
 * Lazy version of map_term_env_if
 *)
let rec map_term_env_if_lazy p f d (env : env) (a : 'a) (trm : types) : types =
  let map_rec = map_term_env_if_lazy p f d in
  let trm' =
    match kind trm with
    | Cast (c, k, t) ->
       let c' = map_rec env a c in
       let t' = map_rec env a t in
       mkCast (c', k, t')
    | Prod (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_local (n, t') env) (d a) b in
       mkProd (n, t', b')
    | Lambda (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_local (n, t') env) (d a) b in
       mkLambda (n, t', b')
    | LetIn (n, trm, typ, e) ->
       let trm' = map_rec env a trm in
       let typ' = map_rec env a typ in
       let e' = map_rec (push_let_in (n, e, typ') env) (d a) e in
       mkLetIn (n, trm', typ', e')
    | App (fu, args) ->
       let fu' = map_rec env a fu in
       let args' = Array.map (map_rec env a) args in
       mkApp (fu', args')
    | Case (ci, ct, m, bs) ->
       let ct' = map_rec env a ct in
       let m' = map_rec env a m in
       let bs' = Array.map (map_rec env a) bs in
       mkCase (ci, ct', m', bs')
    | Fix ((is, i), (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
       mkFix ((is, i), (ns, ts', ds'))
    | CoFix (i, (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
       mkCoFix (i, (ns, ts', ds'))
    | Proj (pr, c) ->
       let c' = map_rec env a c in
       mkProj (pr, c')
    | _ ->
       trm
  in if p env a trm' then f env a trm' else trm'

(*
 * Map a function over a term where the environment doesn't matter
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the argument of type 'a using the a supplied update function
 * Return a new term
 *)
let map_term_if p f d a trm : types =
  map_term_env_if (fun _ a t -> p a t) (fun _ a t -> f a t) d empty_env a trm

(* Lazy version *)
let map_term_if_lazy p f d =
  map_term_env_if_lazy
    (fun _ a t -> p a t)
    (fun _ a t -> f a t)
    d
    empty_env
                  
(*
 * Map a function over subterms of a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
let rec map_subterms_env_if p f d env a trm : types list =
  let map_rec = map_subterms_env_if p f d in
  if p env a trm then
    f env a trm
  else
    match kind trm with
    | Cast (c, k, t) ->
       let cs' = map_rec env a c in
       let ts' = map_rec env a t in
       combine_cartesian (fun c' t' -> mkCast (c', k, t')) cs' ts'
    | Prod (n, t, b) ->
       let ts' = map_rec env a t in
       let bs' = map_rec (push_rel CRD.(LocalAssum(n, t)) env) (d a) b in
       combine_cartesian (fun t' b' -> mkProd (n, t', b')) ts' bs'
    | Lambda (n, t, b) ->
       let ts' = map_rec env a t in
       let bs' = map_rec (push_rel CRD.(LocalAssum(n, t)) env) (d a) b in
       combine_cartesian (fun t' b' -> mkLambda (n, t', b')) ts' bs'
    | LetIn (n, trm, typ, e) ->
       let trms' = map_rec env a trm in
       let typs' = map_rec env a typ in
       let es' = map_rec (push_rel CRD.(LocalDef(n, e, typ)) env) (d a) e in
       combine_cartesian (fun trm' (typ', e') -> mkLetIn (n, trm', typ', e')) trms' (cartesian typs' es')
    | App (fu, args) ->
       let fus' = map_rec env a fu in
       let argss' = combine_cartesian_append (Array.map (map_rec env a) args) in
       combine_cartesian (fun fu' args' -> mkApp (fu', args')) fus' argss'
    | Case (ci, ct, m, bs) ->
       let cts' = map_rec env a ct in
       let ms' = map_rec env a m in
       let bss' = combine_cartesian_append (Array.map (map_rec env a) bs) in
       combine_cartesian (fun ct' (m', bs') -> mkCase (ci, ct', m', bs')) cts' (cartesian ms' bss')
    | Fix ((is, i), (ns, ts, ds)) ->
       let tss' = combine_cartesian_append (Array.map (map_rec env a) ts) in
       let dss' = combine_cartesian_append (Array.map (map_rec_env_fix_cartesian map_rec d env a ns ts) ds) in
       combine_cartesian (fun ts' ds' -> mkFix ((is, i), (ns, ts', ds'))) tss' dss'
    | CoFix (i, (ns, ts, ds)) ->
       let tss' = combine_cartesian_append (Array.map (map_rec env a) ts) in
       let dss' = combine_cartesian_append (Array.map (map_rec_env_fix_cartesian map_rec d env a ns ts) ds) in
       combine_cartesian (fun ts' ds' -> mkCoFix (i, (ns, ts', ds'))) tss' dss'
    | Proj (p, c) ->
       let cs' = map_rec env a c in
       List.map (fun c' -> mkProj (p, c')) cs'
    | _ ->
       [trm]

(*
 * Map a function over subterms of a term in an environment
 * Only apply the function when a proposition is true
 * Apply the function eagerly, but always recurse
 * Update the environment as you go
 * Update the argument of type 'a using the a supplied update function
 * Return all combinations of new terms
 *)
let rec map_subterms_env_if_combs p f d env a trm : types list =
  let map_rec = map_subterms_env_if_combs p f d in
  let trms = if p env a trm then f env a trm else [trm] in
  flat_map
    (fun trm' ->
      match kind trm' with
      | Cast (c, k, t) ->
         let cs' = map_rec env a c in
         let ts' = map_rec env a t in
         combine_cartesian (fun c' t' -> mkCast (c', k, t')) cs' ts'
      | Prod (n, t, b) ->
         let ts' = map_rec env a t in
         let bs' = map_rec (push_rel CRD.(LocalAssum(n, t)) env) (d a) b in
         combine_cartesian (fun t' b' -> mkProd (n, t', b')) ts' bs'
      | Lambda (n, t, b) ->
         let ts' = map_rec env a t in
         let bs' = map_rec (push_rel CRD.(LocalAssum(n, t)) env) (d a) b in
         combine_cartesian (fun t' b' -> mkLambda (n, t', b')) ts' bs'
      | LetIn (n, trm, typ, e) ->
         let trms' = map_rec env a trm in
         let typs' = map_rec env a typ in
         let es' = map_rec (push_rel CRD.(LocalDef(n, e, typ)) env) (d a) e in
         combine_cartesian (fun trm' (typ', e') -> mkLetIn (n, trm', typ', e')) trms' (cartesian typs' es')
      | App (fu, args) ->
         let fus' = map_rec env a fu in
         let argss' = combine_cartesian_append (Array.map (map_rec env a) args) in
         combine_cartesian (fun fu' args' -> mkApp (fu', args')) fus' argss'
      | Case (ci, ct, m, bs) ->
         let cts' = map_rec env a ct in
         let ms' = map_rec env a m in
         let bss' = combine_cartesian_append (Array.map (map_rec env a) bs) in
         combine_cartesian (fun ct' (m', bs') -> mkCase (ci, ct', m', bs')) cts' (cartesian ms' bss')
      | Fix ((is, i), (ns, ts, ds)) ->
         let tss' = combine_cartesian_append (Array.map (map_rec env a) ts) in
         let dss' = combine_cartesian_append (Array.map (map_rec_env_fix_cartesian map_rec d env a ns ts) ds) in
         combine_cartesian (fun ts' ds' -> mkFix ((is, i), (ns, ts', ds'))) tss' dss'
      | CoFix (i, (ns, ts, ds)) ->
         let tss' = combine_cartesian_append (Array.map (map_rec env a) ts) in
         let dss' = combine_cartesian_append (Array.map (map_rec_env_fix_cartesian map_rec d env a ns ts) ds) in
         combine_cartesian (fun ts' ds' -> mkCoFix (i, (ns, ts', ds'))) tss' dss'
      | Proj (p, c) ->
         let cs' = map_rec env a c in
         List.map (fun c' -> mkProj (p, c')) cs'
      | _ ->
         [trm'])
    trms
                                                  
(*
 * Like map_term_env_if, but make a list of subterm results
 *)
let rec map_term_env_if_list p f d (env : env) (a : 'a) (trm : types) : (env * types) list =
  let map_rec = map_term_env_if_list p f d in
  if p env a trm then
    [(env, f env a trm)]
  else
    match kind trm with
    | Cast (c, k, t) ->
       let c' = map_rec env a c in
       let t' = map_rec env a t in
       List.append c' t'
    | Prod (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_local (n, t) env) (d a) b in
       List.append t' b'
    | Lambda (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_local (n, t) env) (d a) b in
       List.append t' b'
    | LetIn (n, trm, typ, e) ->
       let trm' = map_rec env a trm in
       let typ' = map_rec env a typ in
       let e' = map_rec (push_let_in (n, e, typ) env) (d a) e in
       List.append trm' (List.append typ' e')
    | App (fu, args) ->
       let fu' = map_rec env a fu in
       let args' = Array.map (map_rec env a) args in
       List.append fu' (List.flatten (Array.to_list args'))
    | Case (ci, ct, m, bs) ->
       let ct' = map_rec env a ct in
       let m' = map_rec env a m in
       let bs' = Array.map (map_rec env a) bs in
       List.append ct' (List.append m' (List.flatten (Array.to_list bs')))
    | Fix ((is, i), (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
       List.append (List.flatten (Array.to_list ts')) (List.flatten (Array.to_list ds'))
    | CoFix (i, (ns, ts, ds)) ->
       let ts' = Array.map (map_rec env a) ts in
       let ds' = Array.map (map_rec_env_fix map_rec d env a ns ts) ds in
       List.append (List.flatten (Array.to_list ts')) (List.flatten (Array.to_list ds'))
    | Proj (pr, c) ->
       map_rec env a c
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
 *)
let rec map_subterms_env_if_lazy p f d env a trm : types list =
  let map_rec = map_subterms_env_if_lazy p f d in
  let trms' =
    match kind trm with
    | Cast (c, k, t) ->
       let cs' = map_rec env a c in
       let ts' = map_rec env a t in
       combine_cartesian (fun c' t' -> mkCast (c', k, t')) cs' ts'
    | Prod (n, t, b) ->
       let ts' = map_rec env a t in
       let bs' = map_rec (push_rel CRD.(LocalAssum(n, t)) env) (d a) b in
       combine_cartesian (fun t' b' -> mkProd (n, t', b')) ts' bs'
    | Lambda (n, t, b) ->
       let ts' = map_rec env a t in
       let bs' = map_rec (push_rel CRD.(LocalAssum(n, t)) env) (d a) b in
       combine_cartesian (fun t' b' -> mkLambda (n, t', b')) ts' bs'
    | LetIn (n, trm, typ, e) ->
       let trms' = map_rec env a trm in
       let typs' = map_rec env a typ in
       let es' = map_rec (push_rel CRD.(LocalDef(n, e, typ)) env) (d a) e in
       combine_cartesian (fun trm' (typ', e') -> mkLetIn (n, trm', typ', e')) trms' (cartesian typs' es')
    | App (fu, args) ->
       let fus' = map_rec env a fu in
       let argss' = combine_cartesian_append (Array.map (map_rec env a) args) in
       combine_cartesian (fun fu' args' -> mkApp (fu', args')) fus' argss'
    | Case (ci, ct, m, bs) ->
       let cts' = map_rec env a ct in
       let ms' = map_rec env a m in
       let bss' = combine_cartesian_append (Array.map (map_rec env a) bs) in
       combine_cartesian (fun ct' (m', bs') -> mkCase (ci, ct', m', bs')) cts' (cartesian ms' bss')
    | Fix ((is, i), (ns, ts, ds)) ->
       let tss' = combine_cartesian_append (Array.map (map_rec env a) ts) in
       let dss' = combine_cartesian_append (Array.map (map_rec_env_fix_cartesian map_rec d env a ns ts) ds) in
       combine_cartesian (fun ts' ds' -> mkFix ((is, i), (ns, ts', ds'))) tss' dss'
    | CoFix (i, (ns, ts, ds)) ->
       let tss' = combine_cartesian_append (Array.map (map_rec env a) ts) in
       let dss' = combine_cartesian_append (Array.map (map_rec_env_fix_cartesian map_rec d env a ns ts) ds) in
       combine_cartesian (fun ts' ds' -> mkCoFix (i, (ns, ts', ds'))) tss' dss'
    | Proj (p, c) ->
       let cs' = map_rec env a c in
       List.map (fun c' -> mkProj (p, c')) cs'
    | _ ->
       [trm]
  in flat_map (fun trm' -> if p env a trm' then f env a trm' else [trm']) trms'

(* --- Propositions --- *)

(*
 * Like map_term_env_if, but just return true if the proposition is satisfied,
 * and false otherwise.
 *
 * We can make this even more general and just take a combinator
 * and a mapping function and so on, in the future.
 *)
let rec exists_subterm_env p d (env : env) (a : 'a) (trm : types) : bool =
  let exists p a = List.exists p (Array.to_list a) in
  let map_rec = exists_subterm_env p d in
  if p env a trm then
    true
  else
    match kind trm with
    | Cast (c, k, t) ->
       let c' = map_rec env a c in
       let t' = map_rec env a t in
       c' || t'
    | Prod (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_local (n, t) env) (d a) b in
       t' || b'
    | Lambda (n, t, b) ->
       let t' = map_rec env a t in
       let b' = map_rec (push_local (n, t) env) (d a) b in
       t' || b'
    | LetIn (n, trm, typ, e) ->
       let trm' = map_rec env a trm in
       let typ' = map_rec env a typ in
       let e' = map_rec (push_let_in (n, e, typ) env) (d a) e in
       trm' || typ' || e'
    | App (fu, args) ->
       let fu' = map_rec env a fu in
       let args' = exists (map_rec env a) args in
       fu' || args'
    | Case (ci, ct, m, bs) ->
       let ct' = map_rec env a ct in
       let m' = map_rec env a m in
       let bs' = exists (map_rec env a) bs in
       ct' || m' || bs'
    | Fix ((is, i), (ns, ts, ds)) ->
       let ts' = exists (map_rec env a) ts in
       let ds' = exists (map_rec_env_fix map_rec d env a ns ts) ds in
       ts' || ds'
    | CoFix (i, (ns, ts, ds)) ->
       let ts' = exists (map_rec env a) ts in
       let ds' = exists (map_rec_env_fix map_rec d env a ns ts) ds in
       ts' || ds'
    | Proj (pr, c) ->
       map_rec env a c
    | _ ->
       false

(* exists_subterm_env with an empty environment *)
let exists_subterm p d =
  exists_subterm_env
    (fun _ a t -> p a t)
    d
    empty_env

(* all subterms that match a predicate *)
let all_const_subterms p d a t =
  List.map
    snd
    (map_term_env_if_list
       (fun _ a t -> isConst t && p a t)
       (fun en _ t -> t)
       d
       empty_env
       a
       t)

(* --- Containment --- *)
             
(*
 * Check recursively whether a term contains another term
 *)
let contains_term c trm =
  exists_subterm equal shift c trm
              
(* --- Variations --- *)

(* map env without any a *)
let map_unit_env mapper p f env trm =
  mapper (fun en _ t -> p en t) (fun en _ t -> f en t) (fun _ -> ()) env () trm
         
(* map without any a *)
let map_unit mapper p f trm =
  mapper (fun _ t -> p t) (fun _ t -> f t) (fun _ -> ()) () trm

(* Some simple combinations *)
let map_unit_env_if = map_unit_env map_term_env_if
let map_unit_env_if_lazy = map_unit_env map_term_env_if_lazy
let map_unit_if = map_unit map_term_if
let map_unit_if_lazy = map_unit map_term_if_lazy
