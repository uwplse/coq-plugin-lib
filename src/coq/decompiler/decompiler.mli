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
  (* Proof that x = y if true, y = x if false, goal being transformed. *)
  | Rewrite of env * types * bool * types option
  (* Proof that y = x if true, x = y if false. *)
  | RewriteIn of env * types * types * bool
  | ApplyIn of env * types * types
  | Pose of env * types * Id.t
  (* env, induction arg, binding lists *)
  | Induction of env * types * Id.t list list
  | Reflexivity
  | Symmetry
  | Simpl
  | Left
  | Right
  | Split
  | Revert of Id.t list
  | Exists of env * types
  | Auto
  (* Paste the literal expression into the script. *)
  | Expr of string

(* Represents a tactical proof script as a tree. *)
type tactical =
  (* Semicolon tactical followed by subgoals *)
  | Compose of tact list * (tactical list)
           
(* Parses a tactic string into Coq's semantic tactic. *)
val parse_tac_str : string -> unit Proofview.tactic
             
(* Given a term and a list of tactics to try, decompile a term into an Ltac script.
   Each proofview tactic in the list must be paired with their string representation. *)
val tac_from_term : env ->
                    evar_map ->
                    (env -> evar_map -> constr -> (unit Proofview.tactic * string) list) ->
                    constr ->
                    tactical
  
(* Given a decompiled Ltac script, return its string representation. *)
val tac_to_string : evar_map -> tactical -> Pp.t
