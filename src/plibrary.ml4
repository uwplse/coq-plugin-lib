
DECLARE PLUGIN "plib"

open Decompiler
open Constr
open Names
open Environ
open Assumptions
open Search
open Evd
open Printing
open Reducers
open Stdarg
open Utilities
open Zooming
open Defutils
open Envutils
open Stateutils
open Inference
open Tactics
open Pp
open Ltac_plugin
open Nameutils

open Class_tactics
open Stdarg
open Tacarg
open Py
open Serapi_protocol

(* --- Commands --- *)

(* Decompiles a single term into a tactic list printed to console. *)
let decompile_command trm tacs =
  let (sigma, env) = Pfedit.get_current_context () in
  let sigma, trm = intern env sigma trm in
  let trm = unwrap_definition env trm in
  let opts = List.map (fun s -> (parse_tac_str s, s)) tacs in
  let sigma, script = tac_from_term env sigma (fun _ sigma [] _ -> sigma, opts) trm in
  (* let sigma, goal = infer_type env sigma trm in *)
  (* Feedback.msg_warning (ppx_conv_sexp env sigma goal) *)
  (* Feedback.msg_warning (str "the goal: " ++ Printer.pr_constr_env env sigma goal) *)
  (* Feedback.msg_debug (script) *)
  Feedback.msg_debug (tac_to_string sigma script)

(* --- Vernac syntax --- *)

(* Decompile Command *)
VERNAC COMMAND EXTEND Decompile CLASSIFIED AS SIDEFF
| [ "Decompile" constr(trm) ] ->
   [ decompile_command trm [] ]
| [ "Decompile" constr(trm) "with" string_list(l) ] ->
   [ decompile_command trm l ]
END
