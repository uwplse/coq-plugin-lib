(*
 * Utilities for identity types
 *)

open Constr
open Names
open Environ

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
let identity_term (env : env) (typ : types) : types =
  let id = mkApp (id_prop, Array.make 1 typ) in
  try
    let _ = Typeops.infer env id in id
  with _ -> mkApp (id_typ, Array.make 1 typ)

(* --- Questions about constants --- *)

(* Determine if a term (exactly) applies an identity term *)
let applies_identity (trm : types) : bool =
  match kind trm with
  | App (f, _) ->
     equal f id_prop || equal f id_typ
  | _ ->
     false
 
