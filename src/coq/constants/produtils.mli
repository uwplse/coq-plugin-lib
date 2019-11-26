(*
 * Utilities for prod types
 *)

open Constr

(* --- Constants --- *)

val prod : types
val pair : constr
val prod_rect : constr
val fst : constr
val snd : constr

(* --- Representations --- *)

(*
 * An application of pair
 *)
type pair_app =
  {
    typ1 : constr;
    typ2 : constr;
    trm1 : constr;
    trm2 : constr;
  }

(*
 * Convert between a term and a pair_app
 *)
val apply_pair : pair_app -> constr
val dest_pair : constr -> pair_app

(*
 * An application of prod
 *)
type prod_app =
  {
    typ1 : types;
    typ2 : types;
  }

(*
 * Convert between a term and a prod_app
 *)
val apply_prod : prod_app -> types
val dest_prod : types -> prod_app

(*
 * An application of prod_rect
 *)
type prod_elim =
  {
    to_elim : prod_app;
    p : types;
    proof : constr;
    arg : constr;
  }

(*
 * Convert between a term and a prod_elim
 *)
val elim_prod : prod_elim -> constr
val dest_prod_elim : constr -> prod_elim

(*
 * Projections
 *)
val prod_fst : prod_app -> constr -> constr
val prod_snd : prod_app -> constr -> constr
val prod_projections : prod_app -> constr -> constr * constr

