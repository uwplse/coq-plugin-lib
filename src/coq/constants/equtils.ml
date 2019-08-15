(*
 * Utilities for propositional equality
 *)

open EConstr
open Names
open Apputils

let coq_init_logic =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Logic"; "Init"; "Coq"]))

(* --- Constants --- *)

(* equality *)
let eq : types =
  mkInd (MutInd.make1 (KerName.make2 coq_init_logic (Label.make "eq")), 0)

(* Constructor for quality *)
let eq_refl : types =
  mkConstruct (fst (destInd (Evd.from_env (Global.env ())) eq), 1)
        
(* Symmetric eliminator for equality *)
let eq_ind_r : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_ind_r"))

(* Eliminator for equality *)
let eq_ind : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_ind"))

(* Symmetric eleiminator for equality into type *)
let eq_rec_r : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_rec_r"))

(* Eliminator for equality into type *)
let eq_rec : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_rec"))

(* Symmetry *)
let eq_sym : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_sym"))

(* --- Representations --- *)

(*
 * An application of eq
 *)
type eq_app =
  {
    at_type : types;
    trm1 : types;
    trm2 : types;
  }

(*
 * Make an eq type
 *)
let apply_eq (app : eq_app) : types =
  mkAppl (eq, [app.at_type; app.trm1; app.trm2])

(*
 * Deconstruct an eq type
 *)
let dest_eq sigma (trm : types) : eq_app =
  let [at_type; trm1; trm2] = unfold_args sigma trm in
  { at_type; trm1; trm2 }

(*
 * An application of eq_sym
 *)
type eq_sym_app =
  {
    eq_typ : eq_app;
    eq_proof : types;
  }

(*
 * Make an eq type
 *)
let apply_eq_sym (app : eq_sym_app) : types =
  let eq_typ = app.eq_typ in
  mkAppl (eq_sym, [eq_typ.at_type; eq_typ.trm1; eq_typ.trm2; app.eq_proof])

(*
 * Deconstruct an eq type
 *)
let dest_eq_sym sigma (trm : types) : eq_sym_app =
  let [at_type; trm1; trm2; eq_proof] = unfold_args sigma trm in
  let eq_typ = { at_type; trm1; trm2 } in
  { eq_typ; eq_proof }
    
(*
 * An application of eq_ind
 *)
type eq_ind_app =
  {
    at_type : types;
    p : types;
    trm1 : types;
    trm2 : types;
    h : types;
    b : types;
  }

(*
 * Apply an eq_ind
 *)
let apply_eq_ind (app : eq_ind_app) : types =
  mkAppl (eq_ind, [app.at_type; app.trm1; app.p; app.b; app.trm2; app.h])

(* 
 * Deconstruct an eq_ind
 *)
let dest_eq_ind sigma (trm : types) : eq_ind_app =
  let [at_type; trm1; p; b; trm2; h] = unfold_args sigma trm in
  { at_type; trm1; p; b; trm2; h }

(*
 * An application of eq_refl
 *)
type eq_refl_app =
  {
    typ : types;
    trm : types;
  }

(*
 * Apply an eq_refl
 *)
let apply_eq_refl (app : eq_refl_app) : types =
  mkAppl (eq_refl, [app.typ; app.trm])

(* 
 * Deconstruct an eq_refl
 *)
let dest_eq_refl sigma (trm : types) : eq_refl_app =
  let [typ; trm] = unfold_args sigma trm in
  { typ; trm }

(* --- Questions about constants --- *)

(*
 * Check if a term is (exactly) a rewrite via eq_ind or eq_ind_r
 * Don't consider convertible terms
 *)
let is_rewrite sigma (trm : types) : bool =
  let eq_term = eq_constr sigma trm in
  eq_term eq_ind_r || eq_term eq_ind || eq_term eq_rec_r || eq_term eq_rec
