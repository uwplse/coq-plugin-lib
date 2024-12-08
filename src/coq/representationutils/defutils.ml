(*
 * Utilities for defining terms
 *)

open Constr
open Names
open Evd
open Constrexpr
open Constrextern

(* --- Defining Coq terms --- *)

(* https://github.com/ybertot/plugin_tutorials/blob/master/tuto1/src/simple_declare.ml 

TODO do we need to return the updated evar_map? *)
let edeclare ident poly ~opaque sigma udecl body tyopt imps hook =
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
  let sigma = fst (Typing.type_of env sigma body)
  in
  let sigma =
    if Option.has_some tyopt then
      fst (Typing.type_of env sigma (Option.get tyopt))
    else
      sigma
    in
  let sigma = Evd.minimize_universes sigma in (* todo: is this necessary/bad? *)
  let udecl = UState.default_univ_decl in
  let sigma = (Evd.fold_undefined (fun x _ sigma -> Evd.remove sigma x) sigma) sigma in
  let scope = Locality.Global Locality.ImportDefaultBehavior in
  let kind = Decls.(IsDefinition Definition) in
  let info = Declare.Info.make ~scope ~kind ~udecl ~poly () in
  let cinfo = Declare.CInfo.make ~name:ident ~typ:tyopt () in
  Declare.declare_definition ~info:info ~cinfo:cinfo ~opaque:opaque ~body:body sigma

(* Define a new Coq term *)
let define_term ?typ (n : Id.t) (evm : evar_map) (trm : types) =
  let poly = Attributes.is_universe_polymorphism() in
  let udecl = UState.default_univ_decl in
  let etrm = EConstr.of_constr trm in
  let etyp = Option.map EConstr.of_constr typ in
  edeclare n poly ~opaque:false evm udecl etrm etyp [] None

(* Define a Canonical Structure *)
let define_canonical ?typ (n : Id.t) (evm : evar_map) (trm : types) =
  let poly = Attributes.is_universe_polymorphism() in
  let udecl = UState.default_univ_decl in
  let hook = Declare.Hook.make (fun x -> let open Declare.Hook.S in Canonical.declare_canonical_structure x.dref) in
  let etrm = EConstr.of_constr trm in
  let etyp = Option.map EConstr.of_constr typ in
  edeclare n poly ~opaque:false evm udecl etrm etyp [] (Some hook)

(* --- Converting between representations --- *)

(*
 * See defutils.mli for explanations of these representations.
 *)

(* Intern a term (for now, ignore the resulting evar_map) *)
let intern env sigma t : evar_map * types =
  let (sigma, trm) = Constrintern.interp_constr_evars env sigma t in
  sigma, EConstr.to_constr ~abort_on_undefined_evars:false sigma trm

(* Extern a term *)
let extern env sigma t : constr_expr =
  Constrextern.extern_constr ~lax:true env sigma (EConstr.of_constr t)

(* Construct the external expression for a definition *)
let expr_of_global (g : GlobRef.t) : constr_expr =
  let r = extern_reference Id.Set.empty g in
  CAst.make @@ (CAppExpl ((None, r, None), []))

(* Convert a term into a global reference with universes (or raise Not_found) *)
let pglobal_of_constr term =
  let open GlobRef in
  match Constr.kind term with
  | Const (const, univs) -> ConstRef const, univs
  | Ind (ind, univs) -> IndRef ind, univs
  | Construct (cons, univs) -> ConstructRef cons, univs
  | Var id -> VarRef id, Univ.Instance.empty
  | _ -> raise Not_found

(* Convert a global reference with universes into a term *)
let constr_of_pglobal (glob, univs) =
  let open GlobRef in
  match glob with
  | ConstRef const -> mkConstU (const, univs)
  | IndRef ind -> mkIndU (ind, univs)
  | ConstructRef cons -> mkConstructU (cons, univs)
  | VarRef id -> mkVar id

(* Safely instantiate a global reference, with proper universe handling *)
let new_global sigma gref =
  let sigma, typ = Evarutil.new_global sigma gref in
  sigma, EConstr.to_constr ~abort_on_undefined_evars:false sigma typ
