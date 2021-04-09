
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

(*
 * This is a minimal, sound version of the decompiler with our own heuristics
 * and improvements disabled. We can add those back later, but I think it's
 * easier to start with this to experiment with the decompiler.
 *)
  
(* Monadic bind on option types. *)
let (>>=) = Option.bind

(* Alternative (monad plus) operator on functions of
   the form a' -> b' -> c' option. *)
let (<|>) f g x y =
  match f x y, g x y with
  | Some z, _ -> Some z
  | None, z -> z


(* Convert a tactic expression into a semantic tactic. *)
let parse_tac_str (s : string) : unit Proofview.tactic =
  let raw = Pcoq.parse_string Pltac.tactic s in
  let glob = Tacintern.intern_pure_tactic (Tacintern.make_empty_glob_sign ()) raw in
  Tacinterp.eval_tactic glob

(* Run a coq tactic against a given goal, returning generated subgoals *)
let run_tac env sigma (tac : unit Proofview.tactic) (goal : constr)
    : Goal.goal list * Evd.evar_map =
  let p = Proof.start sigma [(env, EConstr.of_constr goal)] in
  let (p', _) = Proof.run_tactic env tac p in
  let (subgoals, _, _, _, sigma) = Proof.proof p' in
  subgoals, sigma
    
(* Returns true if the given tactic solves the goal. *)
let solves env sigma (tac : unit Proofview.tactic) (goal : constr) : bool state =
  try
    let subgoals, sigma = run_tac env sigma tac goal in
    sigma, subgoals = []
  with _ -> sigma, false

(* Compute the type of a term if possible, otherwise None. *)
let type_of env (trm : constr) sigma : (types option) state =
  try
    let sigma, typ = Inference.infer_type env sigma trm in
    sigma, Some typ
  with _ ->
    sigma, None
              
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
  (* Semicolon tactical followed by subgoals, semicolons are
     stored in reverse order to push in constant time. *)
  | Compose of tact list * (tactical list)

(* Return the string representation of a single tactic. *)
let show_tactic sigma tac : Pp.t =
  let prnt e = Printer.pr_constr_env e sigma in
  match tac with
  | Intros ns ->
     let s = if List.tl ns == [] then "intro" else "intros" in
     let names = String.concat " " (List.map Id.to_string ns) in
     str (s ^ " " ^ names)
  | Apply (env, trm) ->
     str "apply " ++ prnt env trm
  | Rewrite (env, trm, left, _) ->
     let s = prnt env trm in
     let arrow = if left then "<- " else "" in
     str ("rewrite " ^ arrow) ++ s
  | RewriteIn (env, prf, hyp, left) ->
     let prf_s, hyp_s = prnt env prf, prnt env hyp in
     let arrow = if left then "" else "<- " in
     str ("rewrite " ^ arrow) ++ prf_s ++ str " in " ++ hyp_s
  | ApplyIn (env, prf, hyp) ->
     let prf_s, hyp_s = prnt env prf, prnt env hyp in
     str "apply " ++ prf_s ++ str " in " ++ hyp_s
  | Pose (env, hyp, n) ->
     let n = str (Id.to_string n) in
     str "pose " ++ prnt env hyp ++ str " as " ++ n
  | Induction (env, trm, names) ->
     let to_s ns = if ns == [] then " " (* prevent "||" *)
                   else String.concat " " (List.map Id.to_string ns) in
     let bindings = str (String.concat "|" (List.map to_s names)) in
     str "induction " ++ prnt env trm ++
       str " as [" ++ bindings ++ str "]"
  | Reflexivity -> str "reflexivity"
  | Simpl -> str "simpl"
  | Left -> str "left"
  | Right -> str "right"
  | Split -> str "split"
  | Revert ns ->
     let names = String.concat " " (List.rev_map Id.to_string ns) in
     str ("revert " ^ names)
  | Symmetry -> str "symmetry"
  | Exists (env, trm) ->
     str "exists " ++ prnt env trm
  | Auto -> str "auto"
  | Expr s -> str s

(* Option monad over function application. *)
let try_app (trm : constr) : (constr * constr array) option =
  match kind trm with
  | App (f, args) -> Some (f, args)
  | _ -> None

(* Option monad over relative indices. *)
let try_rel (trm : constr) : int option =
  match kind trm with
  | Rel i -> Some i
  | _ -> None

(* Option monad over global Gallina-variable. *)
let try_name env (trm : constr) : string option =
  match kind trm with
  | Rel i ->
     let n = expect_name (fst (rel_name_type (lookup_rel i env))) in
     Some (Id.to_string n)
  | Const (c, u) ->
     let ker_name = Constant.canonical c in
     Some (KerName.to_string ker_name)
  | _ -> None

(* Monadic guard for option. *)
let guard (b : bool) : unit option =
  if b then Some () else None

(* Single dotted tactic. *)
let dot sigma tac next : tactical state option = Some (sigma, Compose ([ tac ], [ next ]))

(* Single tactic to finish proof. *)
let qed sigma tac = Some (sigma, Compose ([ tac ], []))

(* Given the list of tactics and their corresponding string
   expressions, try to solve the goal (type of trm),
   return None otherwise. *)
let try_solve env sigma opts trm =
  try
    let goal = (Typeops.infer env trm).uj_type in
    let rec aux sigma opts =
      match opts with
      | [] -> None
      | (tac, expr) :: opts' ->
         let sigma, solved = solves env sigma tac goal in
         if solved
         then Some (Expr expr)
         else aux sigma opts'
    in aux sigma opts
  with _ -> None

(* Generates an apply tactic with implicit arguments if possible. *)
let apply_implicit env sigma trm : tactical state option =
  try
    try_app trm >>= fun (f, args) ->
    try_name env f >>= fun name ->
    let s = String.concat " " [ "apply" ; name ] in
    let opt = parse_tac_str s in
    try_solve env sigma [ (opt, s) ] trm >>= fun tac ->
    qed sigma tac
  with _ -> None

(* Performs the bulk of decompilation on a proof term.
   Opts are the optional goal solving tactics that can be inserted into
     the generated script. If one of these tactics solves the focused goal or 
     can be used intermediately, use the provided string representation of that tactic.
   Returns a list of tactics. *)
let rec first_pass
          (env : env)
          (sigma : Evd.evar_map)
          (get_hints : env -> Evd.evar_map -> constr -> (unit Proofview.tactic * string) list state)
          (trm : constr) : tactical state =
  (* Apply single reduction to terms that *might*
       be in eta expanded form. *)
  let trm = Reduction.whd_betaiota env trm in
  let sigma, hints = get_hints env sigma trm in
  let custom = try_custom_tacs env sigma get_hints hints trm in
  if Option.has_some custom then Option.get custom
  else
    let def = Option.default (sigma, Compose ([ Apply (env, trm) ], []))
                (apply_implicit env sigma trm) in
    let choose f x =
      Option.default def (f x (env, sigma, get_hints)) in
    match kind trm with
    (* "fun x => ..." -> "intro x." *)
    | Lambda (n, t, b) ->
       let (env', trm', names) = zoom_lambda_names env 0 trm in
       let sigma, rest = first_pass env' sigma get_hints trm' in
       sigma, Compose ([ Intros names ], [ rest ])
    (* Match on well-known functions used in the proof. *)
    | App (f, args) ->
       choose (rewrite <|> induction <|> left <|> right <|> split
               <|> reflexivity <|> symmetry <|> exists) (f, args)
    (* Hypothesis transformations or generation tactics. *)
    | LetIn (n, valu, typ, body) ->
       choose (rewrite_in <|> apply_in <|> pose) (n, valu, typ, body)
    (* Remainder of body, simply apply it. *)
    | _ -> def

(* If successful, uses a custom tactic and decompiles subterms solving
   any generated subgoals. *)
and try_custom_tacs env sigma get_hints all_opts trm : tactical state option =
  guard (not (isLambda trm)) >>= fun _ ->
  try
    let goal = (Typeops.infer env trm).uj_type  in
    let goal_env env sigma g =
      let typ = EConstr.to_constr sigma (Goal.V82.abstract_type sigma g) in
      Zooming.zoom_product_type (Environ.reset_context env) typ in
    let rec aux opts : tactical state option =
      match opts with
      | [] -> None
      | (tac, expr) :: opts' ->
         try 
           let subgoals, sigma = run_tac env sigma tac goal in
           let subgoals = List.map (goal_env env sigma) subgoals in
           if subgoals = []
           then (* Goal solving *)
             Some (sigma, Compose ([ Expr expr ], []))
           else
             let new_env = fst (List.hd subgoals) in
             let sigma, same_env = Envutils.compare_envs env new_env sigma in
             if equal goal (snd (List.hd subgoals)) && same_env
             then (* Both goal and context are unchanged *)
               aux opts'
             else (* Intermediate goal generating or context modifying tactic *)
               (* NOTE: These produce a distinct sigma for each subgoal. So we
                        return the latest sigma (from compare_envs) here in the end. *)
               let subterms = List.map (fun (env', goal) ->
                                  (Typehofs.subterms_with_type env sigma goal trm, env'))
                                subgoals in
               (* could not find subterms to satisfy all subgoals? *)
               if List.exists (fun x -> fst x = []) subterms
               then aux opts'
               else
                 (* doesn't matter which subterm we found, it's a proof of the subgoal *)
                 let subterms = List.map (fun (g, e) -> (list_snd g, e)) subterms in
                 let proofs = List.map (fun ((sigma, (_, trm)), env') ->
                                  snd (first_pass env' sigma get_hints trm)) subterms in
                 Some (sigma, Compose ([ Expr expr ], proofs))
         with _ -> aux opts'
    in aux all_opts
  with e -> (* raise e *) None
          
(* Application of a equality eliminator. *)
and rewrite (f, args) (env, sigma, opts) : tactical state option =
  let fx = mkApp (f, args) in
  dest_rewrite fx >>= fun rewr ->
  let sigma, goal = type_of env fx sigma in
  let sigma, rest = first_pass env sigma opts rewr.px in
  dot sigma (Rewrite (env, rewr.eq, rewr.left, goal)) rest

(* Applying an eliminator for induction on a hypothesis in context. *)
and induction (f, args) (env, sigma, opts) : tactical state option =
  guard (is_elim env f) >>= fun _ ->
  guard (not (is_rewrite f)) >>= fun _ ->
  let app = mkApp (f, args) in
  let sigma, ind = deconstruct_eliminator env sigma app in
  inductive_of_elim env (destConst f) >>= fun from_i ->
  let from_m = lookup_mind from_i env in
  let ari = arity (type_of_inductive env 0 from_m) in
  let ind_pos = ari - List.length ind.pms in
  if ind_pos >= List.length ind.final_args
  then
    (Feedback.msg_info (str "Failed to decompile induction");
     Printing.debug_term env app "term";
     None)
  else 
    let ind_var = List.nth ind.final_args ind_pos in
    let forget  = List.length ind.final_args - ind_pos - 1 in
    let zoom_but = arity ind.p - 1 in
    (* Take final args after inducted value, and revert them. *)
    let rev_idx = filter_map try_rel (take forget (List.rev ind.final_args)) in
    let idx_to_name i = expect_name (fst (rel_name_type (lookup_rel i env))) in
    let reverts = List.map idx_to_name rev_idx in
    (* Compute bindings and goals for each case. *)
    let zooms = List.map (zoom_lambda_names env zoom_but) ind.cs in
    let names = List.map (fun (_, _, names) -> names) zooms in
    let sigma, goals = map_state (fun (env, trm, _) sigma -> first_pass env sigma opts trm) zooms sigma in
    let ind = Compose ([ Induction (env, ind_var, names) ], goals) in
    if reverts == [] then Some (sigma, ind) else dot sigma (Revert reverts) ind
    
(* Choose left proof to construct or. *)
and left (f, args) (env, sigma, opts) : tactical state option =
  dest_or_introl (mkApp (f, args)) >>= fun args ->
  let sigma, rest = first_pass env sigma opts args.ltrm in
  dot sigma (Left) rest

(* Choose right proof to construct or. *)
and right (f, args) (env, sigma, opts) : tactical state option =
  dest_or_intror (mkApp (f, args)) >>= fun args ->
  let sigma, rest = first_pass env sigma opts args.rtrm in
  dot sigma (Right) rest

(* Branch two goals as arguments to conj. *)
and split (f, args) (env, sigma, opts) : tactical state option =
  dest_conj (mkApp (f, args)) >>= fun args ->
  let sigma, lhs = first_pass env sigma opts args.ltrm in
  let sigma, rhs = first_pass env sigma opts args.rtrm in
  Some (sigma, Compose ([ Split ], [ lhs ; rhs ]))

(* Converts "apply eq_refl." into "reflexivity." *)
and reflexivity (f, args) (_, sigma, _) : tactical state option =
  dest_eq_refl_opt (mkApp (f, args)) >>= fun _ ->
  qed sigma Reflexivity
  
(* Transform x = y to y = x. *)
and symmetry (f, args) (env, sigma, opts) : tactical state option =
  guard (equal f eq_sym) >>= fun _ ->
  let sym = dest_eq_sym (mkApp (f, args)) in
  let sigma, rest = first_pass env sigma opts sym.eq_proof in
  dot sigma (Symmetry) rest

(* Provide evidence for dependent pair.  *)
and exists (f, args) (env, sigma, opts) : tactical state option =
  guard (equal f Sigmautils.existT) >>= fun _ ->
  let exT = Sigmautils.dest_existT (mkApp (f, args)) in
  let sigma, rest = first_pass env sigma opts exT.unpacked in
  dot sigma (Exists (env, exT.index)) rest
  
(* Value must be a rewrite on a hypothesis in context. *)
and rewrite_in (_, valu, _, body) (env, sigma, opts) : tactical state option =
  let valu = Reduction.whd_betaiota env valu in
  try_app valu                   >>= fun (f, args) ->
  dest_rewrite (mkApp (f, args)) >>= fun rewr -> 
  try_rel rewr.px                >>= fun idx ->
  guard (noccurn (idx + 1) body) >>= fun _ ->
  let n, t = rel_name_type (lookup_rel idx env) in
  let env' = push_local (n, t) env in
  let sigma, rest = first_pass env' sigma opts body in
  dot sigma (RewriteIn (env, rewr.eq, rewr.px, rewr.left)) rest

(* Value must be an application with last argument in context. *)
and apply_in (n, valu, typ, body) (env, sigma, opts) : tactical state option =
  let valu = Reduction.whd_betaiota env valu in
  try_app valu >>= fun (f, args) ->
  let len = Array.length args in
  let hyp = args.(len - 1) in
  try_rel hyp >>= fun idx ->                       (* let H' := F H *)
  guard (noccurn (idx + 1) body) >>= fun _ ->      (* H does not occur in body *)
  guard (not (noccurn 1 body)) >>= fun _ ->        (* new binding DOES occur *)
  let n, t = rel_name_type (lookup_rel idx env) in (* "H" *)
  let env' = push_local (n, t) env in              (* change type of "H" *)
  let prf = mkApp (f, Array.sub args 0 (len - 1)) in
  (* let H2 := f H1 := H2 ... *)
  let apply_binding app_in (_, sigma) =
    try_app body   >>= fun (f, args) ->
    try_rel f      >>= fun i ->
    guard (i == 1) >>= fun _ ->
    let sigma, f' = first_pass env' sigma opts f in
    let sigma, args' = map_state (fun trm sigma -> first_pass env' sigma opts trm)
                         (Array.to_list args) sigma in
    Some (sigma, Compose ([ ApplyIn (env, prf, hyp) ], f' :: args'))
  in 
  (* all other cases *)
  let default app_in (_, sigma) =
    let sigma, rest = first_pass env' sigma opts body in
    dot sigma (ApplyIn (env, prf, hyp)) rest
  in
  (apply_binding <|> default) () (env', sigma)
    
(* Last resort decompile let-in as a pose.  *)
and pose (n, valu, t, body) (env, sigma, opts) : tactical state option =
  let n' = fresh_name env n in
  let env' = push_let_in (Name n', valu, t) env in
  let sigma, decomp_body = first_pass env' sigma opts body in
  (* If the binding is NEVER used, just skip this. *)
  if noccurn 1 body
  then Some (sigma, decomp_body)
  else dot sigma (Pose (env, valu, n')) (decomp_body)
  
(* Decompile a term into its equivalent tactic list. *)
let tac_from_term env sigma get_hints trm : tactical state =
  first_pass env sigma get_hints trm
  
(* Generate indentation space before bullet. *)
let indent level =
  let spacing level = (level - 2) / 3 + 2 in
  let rec aux i = if i <= 1 then 0
                  else spacing i + aux (i - 1)
  in str (String.make (aux level) ' ')

(* Make bullets in order of: -, +, *, --, ++, **, ---, etc., 
   -1 means o indent *)
let bullet level =
  let num = (level + 2) / 3 in
  let blt = match level mod 3 with
    | 0 -> '*'
    | 1 -> '-'
    | 2 -> '+' in
  str (String.make num blt) ++ str " "
  
(* Concatenate list of pp.t with separator *)
let pp_concat sep xs =
  let rec aux xs =
    match xs with
    | [] -> []
    | x :: [] -> [ x ]
    | x :: xs' -> x :: sep :: aux xs'
  in seq (aux xs)
  
(* Show tactical, composed of many tactics. *)
let rec show_tactical sigma (level : int) (bulletted : bool) (t : tactical) : Pp.t =
  let full_indent = if bulletted
                    then indent level ++ bullet level
                    else indent (level + 1) in
  let f i = show_tactical sigma (level + 1) (level + 1 > 0) in
  match t with
  | Compose (tacs, goals) ->
     let to_semi = List.rev (List.map (show_tactic sigma) tacs) in
     let tac_s = full_indent ++ pp_concat (str "; ") to_semi ++ str ".\n" in
     tac_s ++ match goals with
              | [ goal ] ->  show_tactical sigma level false goal
              | goals -> seq (List.mapi f goals)
                
(* Represent tactics as a string. *)
let tac_to_string sigma = show_tactical sigma 0 false
  
