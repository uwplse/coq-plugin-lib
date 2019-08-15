(*
 * Utilities for identity types
 *)

open EConstr
open Names
open Environ
open Evd

let coq_init_datatypes =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Datatypes"; "Init"; "Coq"]))

(* --- Constants --- *)

(* Identity over Prop *)
let id_prop : types =
  mkConst (Constant.make2 coq_init_datatypes (Label.make "idProp"))

(* Identity over Type *)
let id_typ : types =
  mkConst (Constant.make2 coq_init_datatypes (Label.make "id"))
       
(* --- Representations --- *)

(* Get the Coq identity term for typ *)
let identity_term env sigma typ : evar_map * types =
  let sigma, sort_family = Inference.infer_sort env sigma typ in
  match sort_family with
  | InProp ->
     (sigma, mkApp (id_prop, Array.make 1 typ))
  | _ ->
     (sigma, mkApp (id_typ, Array.make 1 typ))

(* --- Questions about constants --- *)

(* Determine if a term (exactly) applies an identity term *)
let applies_identity sigma (trm : types) : bool =
  match kind sigma trm with
  | App (f, _) ->
     eq_constr sigma f id_prop || eq_constr sigma f id_typ
  | _ ->
     false
 
