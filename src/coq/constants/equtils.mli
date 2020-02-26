(*
 * Utilities for propositional equality
 *)

open Constr

(* --- Constants --- *)

val eq : types
val eq_refl : types
val eq_ind_r : types
val eq_ind : types
val eq_rec_r : types
val eq_rec : types
val eq_rect_r : types
val eq_rect : types
val eq_sym : types

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
 * Convert between a term and an eq_app
 *)
val apply_eq : eq_app -> types
val dest_eq : types -> eq_app

(*
 * An application of eq_sym
 *)
type eq_sym_app =
  {
    eq_typ : eq_app;
    eq_proof : types;
  }

(*
 * Convert between a term and an eq_sym_app
 *)
val apply_eq_sym : eq_sym_app -> types
val dest_eq_sym : types -> eq_sym_app

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
 * Convert between a term and an eq_app
 *)
val apply_eq_ind : eq_ind_app -> types
val dest_eq_ind : types -> eq_ind_app
                                
(*
 * An application of eq_refl
 *)
type eq_refl_app =
  {
    typ : types;
    trm : types;
  }

(*
 * Convert between a term and an eq_refl
 *)
val apply_eq_refl : eq_refl_app -> types
val dest_eq_refl : types -> eq_refl_app

(* --- Questions about constants --- *)

(* Check if a term is eq_ind, eq_rec, or eq_rect *)
val is_rewrite_l : types ->  bool
  
(* Check if a term is eq_ind_r, eq_rec_r, or eq_rect_r *)
val is_rewrite_r : types ->  bool
  
(*
 * Check if a term is a (exactly) rewrite via eq_ind or eq_ind_r etc.
 * Don't consider convertible terms
 *)
val is_rewrite : types -> bool
