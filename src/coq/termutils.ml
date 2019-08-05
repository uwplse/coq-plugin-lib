(*
 * Coq term and environment management
 *)

open Util
open Context
open Environ
open Constr
open Names
open Constrexpr
open Evd
open Utilities
open Decl_kinds
open Constrextern
open Recordops

module Globmap = Globnames.Refmap
module Globset = Globnames.Refset
                                                                    
(* --- Representations --- *)

(*
 * See termutils.mli for explanations of these representations.
 *)

(* Intern a term (for now, ignore the resulting evar_map) *)
let intern env evd t : types =
  let (trm, _) = Constrintern.interp_constr env evd t in
  EConstr.to_constr evd trm

(* Extern a term *)
let extern env evd t : constr_expr =
  Constrextern.extern_constr true env evd (EConstr.of_constr t)

(*
 * Construct the external expression for a definition. 
 *)
let expr_of_global (g : global_reference) : constr_expr =
  let r = extern_reference Id.Set.empty g in
  CAst.make @@ (CAppExpl ((None, r, None), []))

(* Convert a term into a global reference with universes (or raise Not_found) *)
let pglobal_of_constr term =
  match Constr.kind term with
  | Const (const, univs) -> ConstRef const, univs
  | Ind (ind, univs) -> IndRef ind, univs
  | Construct (cons, univs) -> ConstructRef cons, univs
  | Var id -> VarRef id, Univ.Instance.empty
  | _ -> raise Not_found

(* Convert a global reference with universes into a term *)
let constr_of_pglobal (glob, univs) =
  match glob with
  | ConstRef const -> mkConstU (const, univs)
  | IndRef ind -> mkIndU (ind, univs)
  | ConstructRef cons -> mkConstructU (cons, univs)
  | VarRef id -> mkVar id

(* --- Convertibility and reduction --- *)

(* Safely instantiate a global reference, with proper universe handling (TODO move) *)
let e_new_global evm gref =
  Evarutil.e_new_global evm gref |> EConstr.to_constr !evm

(* --- Environments (TODO rename) --- *)

(* Is the named declaration an assumption? *)
let is_named_assum = Named.Declaration.is_local_assum

(* Is the named declaration a definition? *)
let is_named_defin = Named.Declaration.is_local_def

(* Make the named declaration for a local assumption *)
let named_assum (id, typ) =
  Named.Declaration.LocalAssum (id, typ)

(* Make the named declaration for a local definition *)
let named_defin (id, def, typ) =
  Named.Declaration.LocalDef (id, def, typ)

(* Get the name of a named declaration *)
let named_ident decl =
  Named.Declaration.get_id decl

(* Get the optional value of a named declaration *)
let named_value decl =
  Named.Declaration.get_value decl

(* Get the type of a named declaration *)
let named_type decl =
  Named.Declaration.get_type decl

(* Map over a named context with environment kept in synch *)
let map_named_context env make ctxt =
  Named.fold_outside
    (fun decl (env, res) ->
       push_named decl env, (make env decl) :: res)
    ctxt
    ~init:(env, []) |>
    snd

(* Append two contexts (inner first, outer second), shifting internal indices. *)
let context_app inner outer =
  List.append
    (Termops.lift_rel_context (Rel.length outer) inner)
    outer

(* Bind the declarations of a local context as product/let-in bindings *)
let recompose_prod_assum decls term =
  let bind term decl = Term.mkProd_or_LetIn decl term in
  Rel.fold_inside bind ~init:term decls

(* Bind the declarations of a local context as lambda/let-in bindings *)
let recompose_lam_assum decls term =
  let bind term decl = Term.mkLambda_or_LetIn decl term in
  Rel.fold_inside bind ~init:term decls

(* --- Basic questions about terms --- *)

(* Is the first term equal to a "head" (application prefix) of the second?
 * The notion of term equality is syntactic (i.e., no environment) and defaults
 * to syntactic equality modulo alpha, casts, grouping, and universes. The
 * result of this function is an informative boolean: an optional array, with
 * None meaning false and Some meaning true and giving the trailing arguments.
 *
 * This function is similar to is_or_applies, except for term equality and the
 * informative boolean result.
 *
 * TODO move, maybe don't expose
 *)
let eq_constr_head ?(eq_constr=eq_constr_nounivs) term term' =
  let head, args = decompose_app term in
  let head', args' = decompose_app term' in
  if eq_constr head head' && List.prefix_of eq_constr args args' then
    Some (List.skipn (List.length args) args' |> Array.of_list)
  else
    None

(* --- Names --- *)

type global_substitution = global_reference Globmap.t

(* Substitute global references throughout a term *)
let subst_globals subst term =
  let rec aux term =
    try
      pglobal_of_constr term |>
      map_puniverses (flip Globmap.find subst) |>
      constr_of_pglobal
    with Not_found ->
      Constr.map aux term
  in
  aux term
