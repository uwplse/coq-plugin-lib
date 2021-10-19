open Names
open Constr
open Environ
open Envutils
open Pp
open Equtils
open Proputils
open Indutils
open Funutils
open Inference
open Vars
open Utilities
open Zooming
open Nameutils
open Ltac_plugin
open Stateutils
open Lymp
open Ser_names
open Ser_environ
open Ser_goal

let py = init "."
let agent_utils = Lymp.get_module py "agent_utils"
let prover = Lymp.get_module py "prover"

(* Follow this format *)
(* let module = Lymp.get_module py <module_name> in
let obj = Lymp.get_ref module <object_name> <args> in
let result = Lymp.get_<return_type> obj <method_name> <args> in *)

let parse_script script = 
	get_list agent_utils "parse_script" [Pylist script]

let import_model () = 
	get_ref agent_utils "import_model" []

let beam_search env prev goal = 
	let model = import_model () in 
	let script = parse_script prev in (* need to reverse prev *)
	(* ser api calls to serialize *)
	let ser_env = sexp_of_env env in 
	let ser_goal = sexp_of_goal goal in 
	let filter_env = Lymp.get_ref agent_utils "filter_env" [Pyref ser_env] in 
	let local_context, parsed_goal = Lymp.get_ref agent_utils "parse_goal" [Pyref ser_goal] in 
	get_list prover "beam_search" [Pyref model; Pyref filter_env; Pyref local_context; Pyref parsed_goal; Pylist script]
