(*
 * Utilities for defining terms
 *)

open Constr
open Names
open Evd
open Decl_kinds
open Recordops

(* https://github.com/ybertot/plugin_tutorials/blob/master/tuto1/src/simple_declare.ml *)
let edeclare ident (_, poly, _ as k) ~opaque sigma udecl body tyopt imps hook refresh =
  let open EConstr in
  (* XXX: "Standard" term construction combinators such as `mkApp`
     don't add any universe constraints that may be needed later for
     the kernel to check that the term is correct.

     We could manually call `Evd.add_universe_constraints`
     [high-level] or `Evd.add_constraints` [low-level]; however, that
     turns out to be a bit heavyweight.

     Instead, we call type inference on the manually-built term which
     will happily infer the constraint for us, even if that's way more
     costly in term of CPU cycles.

     Beware that `type_of` will perform full type inference including
     canonical structure resolution and what not.
   *)
  let env = Global.env () in
  let sigma =
    if refresh then
      fst (Typing.type_of ~refresh:false env sigma body)
    else
      sigma
  in
  let sigma = Evd.minimize_universes sigma in
  let body = to_constr sigma body in
  let tyopt = Option.map (to_constr sigma) tyopt in
  let uvars_fold uvars c =
    Univ.LSet.union uvars (Univops.universes_of_constr env c) in
  let uvars = List.fold_left uvars_fold Univ.LSet.empty
    (Option.List.cons tyopt [body]) in
  let sigma = Evd.restrict_universe_context sigma uvars in
  let univs = Evd.check_univ_decl ~poly sigma udecl in
  let ubinders = Evd.universe_binders sigma in
  let ce = Declare.definition_entry ?types:tyopt ~univs body in
  DeclareDef.declare_definition ident k ce ubinders imps hook

(* Define a new Coq term *)
let define_term ?typ (n : Id.t) (evm : evar_map) (trm : types) (refresh : bool) =
  let k = (Global, Flags.is_universe_polymorphism(), Definition) in
  let udecl = Univdecls.default_univ_decl in
  let nohook = Lemmas.mk_hook (fun _ x -> x) in
  let etrm = EConstr.of_constr trm in
  let etyp = Option.map EConstr.of_constr typ in
  edeclare n k ~opaque:false evm udecl etrm etyp [] nohook refresh

(* Define a Canonical Structure *)
let define_canonical ?typ (n : Id.t) (evm : evar_map) (trm : types) (refresh : bool) =
  let k = (Global, Flags.is_universe_polymorphism (), CanonicalStructure) in
  let udecl = Univdecls.default_univ_decl in
  let hook = Lemmas.mk_hook (fun _ x -> declare_canonical_structure x; x) in
  let etrm = EConstr.of_constr trm in
  let etyp = Option.map EConstr.of_constr typ in
  edeclare n k ~opaque:false evm udecl etrm etyp [] hook refresh
