open Environ
open Evd
open Names
open Constr
   
(* Abstraction of Coq tactics supported by this decompiler.
   Serves as an intermediate representation that can be either
   transformed into a string or a sequence of actual tactics. *)
type tact =
  | Intros of Id.t list
  | Apply of env * types
  (* Proof that x = y if true, y = x if false. *)
  | Rewrite of env * types * bool
  (* Proof that y = x if true, etc. *)
  | RewriteIn of env * types * types * bool
  | ApplyIn of env * types * types
  | Pose of env * types * Id.t
  (* env, induction arg, binding lists, shared prefix, subgoals *)
  | Induction of env * types * Id.t list list * tact list * tact list list
  | Reflexivity
  | Symmetry
  | Simpl
  | Left
  | Right
  (* shared ";" tactics, left and right subgoals *)
  | Split of tact list * tact list list
  | Revert of Id.t list
  | Exists of env * types
  | Auto
  (* Paste the literal expression into the script. *)
  | Expr of string
          
(* Given a term and a list of tactics to try, decompile a term into an Ltac script.
   Each proofview tactic in the list must be paired with their string representation. *)
val tac_from_term : env -> evar_map -> (unit Proofview.tactic * string) list -> constr -> tact list

(* Given a decompiled Ltac script, return its string representation. *)
val tac_to_string : evar_map -> tact list -> Pp.t
