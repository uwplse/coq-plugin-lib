open Names
open Declarations

(* --- Modules --- *)

(*
 * Pull any functor parameters off the module signature, returning the list of
 * functor parameters and the list of module elements (i.e., fields).
 *)
val decompose_module_signature : module_signature -> (Names.MBId.t * module_type_body) list * structure_body

(*
 * Declare an interactive (i.e., elementwise) module structure, with the
 * functional argument called to populate the module elements by declaration.
 *
 * The optional argument specifies functor parameters.
 *)
val declare_module_structure : ?params:(Declaremods.module_params) -> Id.t -> (ModPath.t -> unit) -> ModPath.t

(*
 * Fold over the constant/inductive definitions within a module structure,
 * skipping any module (type) components and any unsupported (e.g., mutual)
 * inductive definitions.
 *
 * Elimination schemes (e.g., `Ind_rect`) are filtered out from the definitions.
 *)
(* val fold_module_structure_by_decl : 'a -> ('a -> Constant.t -> 'opaque constant_body -> 'a) -> ('a -> inductive -> Inductive.mind_specif -> 'a) -> module_body -> 'a 
val fold_module_structure_by_decl : 'a -> ('a -> Names.Constant.t -> 'opaque Declarations.constant_body -> 'a) -> ('a -> Names.inductive -> Inductive.mind_specif -> 'a) -> Declarations.module_body -> 'a *)
val fold_module_structure_by_decl : 'a -> ('a -> Names.Constant.t -> Opaqueproof.opaque Declarations.constant_body -> 'a) -> ('a -> Names.MutInd.t * int -> Declarations.mutual_inductive_body * Declarations.one_inductive_body -> 'a) -> 'b Declarations.generic_module_body -> 'a

(*
 * Same as `fold_module_structure_by_decl` except a single step function
 * accepting a global reference.
 *)
(* val fold_module_structure_by_glob : 'a -> ('a -> evaluable_global_reference -> 'a) -> module_body -> 'a *)
val fold_module_structure_by_glob : 'a -> ('a -> Names.evaluable_global_reference -> 'a) -> Declarations.module_body -> 'a

(*
 * Same as `fold_module_structure_by_glob` except an implicit unit accumulator.
 *)
val iter_module_structure_by_glob : (Globnames.global_reference -> unit) -> 'a Declarations.generic_module_body -> unit
val fold_module_structure_by_glob : 'a -> ('a -> Globnames.global_reference -> 'a) -> 'b Declarations.generic_module_body -> 'a
