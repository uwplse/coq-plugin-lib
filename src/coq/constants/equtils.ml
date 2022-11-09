(*
 * Utilities for propositional equality
 *)

open Constr
open Names
open Apputils

let coq_init_logic =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Logic"; "Init"; "Coq"]))

(* --- Constants --- *)

(* equality *)
let eq : types =
  mkInd (MutInd.make1 (Names.KerName.make coq_init_logic (Label.make "eq")), 0)

(* Constructor for quality *)
let eq_refl : types =
  mkConstruct (fst (destInd eq), 1)
        
(* Symmetric eliminator for equality *)
let eq_ind_r : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_ind_r"))

(* Eliminator for equality *)
let eq_ind : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_ind"))

(* Symmetric eliminator for equality into type "P : A -> Set" *)
let eq_rec_r : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_rec_r"))

(* Eliminator for equality into type "P : A -> Set" *)
let eq_rec : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_rec"))

(* Symmetric eliminator for equality into type "P : A -> Type" *)
let eq_rect_r : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_rect_r"))
  
(* Eliminator for equality into type "P : A -> Type" *)
let eq_rect : types =
  mkConst (Constant.make2 coq_init_logic (Label.make "eq_rect"))
  
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
let dest_eq (trm : types) : eq_app =
  let [at_type; trm1; trm2] = unfold_args trm in
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
let dest_eq_sym (trm : types) : eq_sym_app =
  let [at_type; trm1; trm2; eq_proof] = unfold_args trm in
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
let dest_eq_ind (trm : types) : eq_ind_app =
  let [at_type; trm1; p; b; trm2; h] = unfold_args trm in
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
let dest_eq_refl (trm : types) : eq_refl_app =
  let [typ; trm] = unfold_args trm in
  { typ; trm }

(*
 * Deconstruct an eq_refl.
 * None on failure, or if not actually an eq_refl.
 *)
let dest_eq_refl_opt (trm : types) : eq_refl_app option =
  match kind trm with
  | App (f, args) ->
     if equal f eq_refl && Array.length args == 2 then
       Some { typ = args.(0) ; trm = args.(1)  }
     else
       None
  | _ -> None   
  
(* --- Questions about constants --- *)

(* Check if a term is eq_ind, eq_rec, or eq_rect *)
let is_rewrite_l (trm : types) : bool =
  let eq_term = equal trm in
  eq_term eq_ind || eq_term eq_rec || eq_term eq_rect

(* Check if a term is eq_ind_r, eq_rec_r, or eq_rect_r *)
let is_rewrite_r (trm : types) : bool =
  let eq_term = equal trm in
  eq_term eq_ind_r || eq_term eq_rec_r || eq_term eq_rect_r

(*
 * Check if a term is (exactly) a rewrite via eq_ind or eq_ind_r etc.
 * Don't consider convertible terms
 *)
let is_rewrite (trm : types) : bool =
  is_rewrite_l trm || is_rewrite_r trm


(* Information required to perform a rewrite. *)
type rewrite_args = {
    a : types;
    x : constr;
    p : constr;
    px : constr;
    y : constr;
    eq : constr;
    params : constr array;
    left : bool
  }

let rewr_app f app =
  let args = [app.a; app.x; app.p; app.px; app.y; app.eq] in
  mkAppl (f, args @ Array.to_list app.params)
                  
let apply_rewrite_ind app =
  let f = if app.left then eq_ind else eq_ind_r in
  rewr_app f app

let apply_rewrite_rec app =
  let f = if app.left then eq_rec else eq_rec_r in
  rewr_app f app
  
let apply_rewrite_rect app =
  let f = if app.left then eq_rect else eq_rect_r in
  rewr_app f app
                  
let dest_rewrite trm : rewrite_args option =
  match kind trm with
  | App (f, args) ->
     if Array.length args >= 6 && is_rewrite f then
       let left = is_rewrite_l f in
       let params = Array.sub args 6 (Array.length args - 6) in
       Some { a = args.(0) ; x = args.(1) ; p = args.(2) ;
              px = args.(3) ; y = args.(4) ; eq = args.(5) ;
              left = left ; params = params } 
     else
       None
  | _ -> None
   
