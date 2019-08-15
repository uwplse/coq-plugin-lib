(*
 * Dealing with arguments of applications for indexing inductive types
 *)

open EConstr
open Utilities
open Debruijn
open Typehofs
open Hofimpls
open Reducers
open Sigmautils
open Apputils
open Evd

(* --- Generic functions --- *)

(*
 * Insert an index into a list of terms in the location index_i
 *)
let insert_index index_i index args =
  let (before, after) = take_split index_i args in
  List.append before (index :: after)

(*
 * Remove an index from a list of terms in the location index_i
 *)
let remove_index index_i args =
  let (before, after) = take_split index_i args in
  List.append before (List.tl after)

(*
 * Insert an index where an old index was
 *)
let reindex index_i index args =
  insert_index index_i index (remove_index index_i args)

(*
 * Reindex using a reindexer, but for an application
 *)
let reindex_app sigma reindexer app =
  mkAppl (first_fun sigma app, reindexer (unfold_args sigma app))

(*
 * Reindex the body of a lambda
 *)
let reindex_body sigma reindexer lam =
  let (n, t, b) = destLambda sigma lam in
  mkLambda (n, t, reindexer sigma b)

(* --- Managing inductive property arguments --- *)

(*
 * Apply the term to a dummy index, when we would like the other arguments,
 * but we are not sure if the term is a lambda or curried
 *)
let dummy_index env sigma f =
  reduce_stateless reduce_term env sigma (mkAppl (f, [mkRel 0]))

(*
 * Unshift arguments after index_i, since the index is no longer in
 * the hypotheses
 *)
let adjust_no_index sigma index_i args =
  let (before, after) = take_split index_i args in
  List.append before (unshift_all sigma after)

(*
 * Returns true if the hypothesis i is used to compute the index at position
 * off in any application of the property p in some inductive hypothesis
 * of the eliminator type typ
 *)
let rec computes_ih_index sigma off p i typ =
  match kind sigma typ with
  | Prod (n, t, b) ->
     let p_b = shift sigma p in
     let i_b = shift sigma i in
     if applies sigma p t then
       let index = get_arg sigma off t in
       contains_term sigma i index || computes_ih_index sigma off p_b i_b b
     else
       computes_ih_index sigma off p_b i_b b
  | _ ->
     false
                 
(* --- Getting arguments to indexed types --- *)

(*
 * Given a type we are promoting to/forgetting from,
 * get all of the arguments to that type that aren't the new/forgotten index
 *)
let non_index_args index_i env sigma typ =
  let typ = reduce_stateless reduce_nf env sigma typ in
  if is_or_applies sigma sigT typ then
    let packer = (dest_sigT sigma typ).packer in
    remove_index index_i (unfold_args sigma (dummy_index env sigma packer))
  else
    unfold_args sigma typ

(*
 * Given a term with the type we are promoting to/forgetting from,
 * get all of the arguments to that type that aren't the new/forgotten index
 *)
let non_index_typ_args index_i env sigma trm =
  if is_or_applies sigma existT trm then
    (* don't bother type-checking *)
    let packer = (dest_existT sigma trm).packer in
    sigma, remove_index index_i (unfold_args sigma (dummy_index env sigma packer))
  else
    on_type
      (fun env sigma typ -> sigma, non_index_args index_i env sigma typ)
      env
      sigma
      trm
