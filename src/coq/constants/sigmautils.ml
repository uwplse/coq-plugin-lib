(*
 * Utilities for sigma types
 *)

open Constr
open Names
open Termutils

let coq_init_specif =
  ModPath.MPfile
    (DirPath.make (List.map Id.of_string ["Specif"; "Init"; "Coq"]))

(* --- Constants --- *)

(* sigma types *)
let sigT : types =
  mkInd (MutInd.make1 (KerName.make2 coq_init_specif (Label.make "sigT")), 0)

(* Introduction for sigma types *)
let existT : types =
  mkConstruct (fst (destInd sigT), 1)

(* Elimination for sigma types *)
let sigT_rect : types =
  mkConst (Constant.make2 coq_init_specif (Label.make "sigT_rect"))

(* Left projection *)
let projT1 : types =
  mkConst (Constant.make2 coq_init_specif (Label.make "projT1"))

(* Right projection *)
let projT2 : types =
  mkConst (Constant.make2 coq_init_specif (Label.make "projT2"))

(* --- Representations --- *)

(*
 * An application of existT
 *)
type existT_app =
  {
    index_type : types;
    packer : types;
    index : types;
    unpacked : types;
  }

(*
 * Pack an existT term from an index type, packer, index, and unpacked version
 *)
let pack_existT (app : existT_app) : types =
  mkAppl (existT, [app.index_type; app.packer; app.index; app.unpacked])

(*
 * Deconstruct an existT term
 *)
let dest_existT (trm : types) : existT_app =
  let [index_type; packer; index; unpacked] = unfold_args trm in
  { index_type; packer; index; unpacked }

(*
 * An application of sigT
 *)
type sigT_app =
  {
    index_type : types;
    packer : types;
  }

(*
 * Pack a sigT type from an index type and a packer
 *)
let pack_sigT (app : sigT_app) =
  mkAppl (sigT, [app.index_type; app.packer])

(*
 * Deconsruct a sigT type from a type
 *)
let dest_sigT (typ : types) =
  let [index_type; packer] = unfold_args typ in
  { index_type; packer }

(*
 * Build the eta-expansion of a term known to have a sigma type.
 *)
let eta_sigT (term : constr) (typ : types) =
  let { index_type; packer } = dest_sigT typ in
  let fst = mkApp (projT1, [|index_type; packer; term|]) in
  let snd = mkApp (projT2, [|index_type; packer; term|]) in
  mkApp (existT, [|index_type; packer; fst; snd|])

(*
 * An application of sigT_rect
 *)
type sigT_elim =
  {
    to_elim : sigT_app;
    packed_type : types;
    unpacked : types;
    arg : types;
  }

(*
 * Eliminate a sigT given an index type, packer, packed type, unpacked term,
 * and the term itself
 *)
let elim_sigT (app : sigT_elim) =
  let index_type = app.to_elim.index_type in
  let packer = app.to_elim.packer in
  let packed_type = app.packed_type in
  let unpacked = app.unpacked in
  let arg = app.arg in
  mkAppl (sigT_rect, [index_type; packer; packed_type; unpacked; arg])

(*
 * Deconstruct an application of sigT_rect
 *)
let dest_sigT_elim (trm : types) =
  let [index_type; packer; packed_type; unpacked; arg] = unfold_args trm in
  let to_elim = { index_type ; packer } in
  { to_elim; packed_type; unpacked; arg }

(*
 * Left projection of a sigma type
 *)
let project_index (app : sigT_app) trm =
  mkAppl (projT1, [app.index_type; app.packer; trm])

(*
 * Right projection of a sigma type
 *)
let project_value (app : sigT_app) trm =
  mkAppl (projT2, [app.index_type; app.packer; trm])

(*
 * Both projections of a sigma type
 *)
let projections (app : sigT_app) trm =
  (project_index app trm, project_value app trm)
