
open Names
open Constr
open Environ
open Envutils
open Tactics
open Pp
open Contextutils
open Equtils
open Apputils
open Proputils
open Indutils
open Funutils
open Inference
open Vars
open Utilities
open Zooming
open Nameutils
   
open Ltac_plugin
open Tacarg
   
(*
 * Only in OCaml 4.08.0 onward, so we implement ourselves
 *)
let filter_map f l =
  let f_somes = List.filter (fun o -> Option.has_some o) (List.map f l) in
  List.map Option.get f_somes

(* Drop the first n elements off a list *)
let rec drop n xs =
  if n == 0
  then xs
  else drop (n - 1) (List.tl xs)
  
(* Compare whether all elements of two lists of equal length are equal. *)
let rec list_eq (cmp : 'a -> 'a -> bool) xs ys : bool =
  match xs, ys with
  | [], [] -> true
  | x :: xs', y :: ys' -> cmp x y && list_eq cmp xs' ys'
  | _, _ -> false
          
(* Compare if all elements of a single list are equal. *)
let all_eq (cmp : 'a -> 'a -> bool) xs : bool =
  match xs with
  | [] -> true
  | x :: xs' -> List.for_all (fun y -> cmp x y) xs'
 
(* Count length of shared prefix between lists. *)
let rec count_shared_prefix (cmp : 'a -> 'a -> bool) xs ys  : int =
  match xs, ys with
  | x :: xs', y :: ys' ->
     if cmp x y then 1 + count_shared_prefix cmp xs' ys' else 0
  | _, _ -> 0
  
(* Monadic bind on option types. *)
let (>>=) = Option.bind

(* Alternative (monad plus) operator on functions of
   the form a' -> b' -> c' option. *)
let (<|>) f g x y =
  match f x y, g x y with
  | Some z, _ -> Some z
  | None, z -> z

            
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
  
(* True if both tactics are "equal" (perform the same effect). *)
let rec compare (tac1 : tact) (tac2 : tact) : bool =
  match tac1, tac2 with
  | Intros ns1, Intros ns2 -> list_eq Id.equal ns1 ns2
  | Apply (_, t1), Apply (_, t2) -> Constr.equal t1 t2
  | Rewrite (_, t1, b1), Rewrite (_, t2, b2) -> b1 == b2 && Constr.equal t1 t2
  | RewriteIn (_, t1, t1', b1), RewriteIn (_, t2, t2', b2) ->
     b1 == b2 && Constr.equal t1 t2 && Constr.equal t1' t2'
  | ApplyIn (_, t1, t1'), ApplyIn (_, t2, t2') ->
     Constr.equal t1 t2 && Constr.equal t1' t2'
  | Pose (_, t1, n1), Pose (_, t2, n2) -> Id.equal n1 n2 && Constr.equal t1 t2
  (* Induction *)
  | Reflexivity, Reflexivity -> true
  | Symmetry, Symmetry -> true
  | Simpl, Simpl -> true
  | Left, Left -> true
  | Right, Right -> true
  (* Split *)
  | Revert ns1, Revert ns2 -> list_eq Id.equal ns1 ns2
  | Exists (_, t1), Exists (_, t2) -> Constr.equal t1 t2
  | Auto, Auto -> true
  | _ -> false

(* The longest tactic list prefixing each subgoal, and their remainders. *)
let rec tact_shared_prefix (goals : tact list list) : tact list * tact list list =
  if goals == [] (* No subgoals *)
     || List.tl goals == [] (* 1 subgoal *)
     || List.exists (fun xs -> xs == []) goals
  then ([], goals)
  else 
    let hs = List.map List.hd goals in
    if all_eq compare hs
    then let ts = List.map List.tl goals in
         let (rest, goals') = tact_shared_prefix ts in
         (List.hd hs :: rest, goals') 
    else ([], goals)

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

(* Monadic guard for option. *)
let guard (b : bool) : unit option =
  if b then Some () else None

(* Inserts "simpl." before every rewrite. *)
let rec simpl (tacs : tact list) : tact list =
  match tacs with
  | [] -> []
  | Rewrite (e, t, l) :: tacs' ->
     Simpl :: Rewrite (e, t, l) :: simpl tacs'
  | Split (prefix, goals) :: tacs' ->
     Split (simpl prefix, List.map simpl goals) :: simpl tacs'
  | tac :: tacs' -> tac :: simpl tacs'
                  
(* Combine adjacent intros and revert tactics if possible. *)
let rec intros_revert (tacs : tact list) : tact list =
  match tacs with
  | Intros xs :: Revert ys :: tacs' ->
     let xs' = List.rev xs in
     let n = count_shared_prefix Id.equal xs' ys in
     Intros (take (List.length xs - n) xs) :: Revert (drop n ys) :: intros_revert tacs'
  | Split (prefix, goals) :: tacs' ->
     Split (intros_revert prefix, List.map intros_revert goals) :: intros_revert tacs'
  | Induction (env, x, ns, prefix, goals) :: tacs' ->
     Induction (env, x, ns, intros_revert prefix, List.map intros_revert goals)
     :: intros_revert tacs'
  | tac :: tacs' -> tac :: intros_revert tacs'
  | [] -> []
                  
(* Remove last name of intros if followed by induction on that hypothesis. *)
let rec intros_induction (tacs : tact list) : tact list =
  (* this is bugged, see intros. vs intro x. *)
  match tacs with
  | Intros [] :: tacs' -> intros_induction tacs'
  | Intros xs :: Induction (env, x, ns, prefix, goals) :: tacs' ->
     let prefix' = intros_induction prefix in
     let goals' = List.map intros_induction goals in
     let tacs'' = intros_induction tacs' in
     let rest = Induction (env, x, ns, prefix', goals') :: tacs'' in
     Option.default (Intros xs :: rest)
       (guard (xs <> []) >>= fun _ ->
        let last = List.hd (List.rev xs) in
        try_rel x >>= fun idx ->
        let n = expect_name (fst (rel_name_type (lookup_rel idx env))) in
        guard (Id.equal last n) >>= fun _ ->
        Some rest)
  | Split (prefix, goals) :: tacs' ->
     Split (intros_induction prefix, List.map intros_induction goals) :: intros_induction tacs'
  | Induction (env, x, ns, prefix, goals) :: tacs' ->
     Induction (env, x, ns, intros_induction prefix, List.map intros_induction goals)
     :: intros_induction tacs'
  | tac :: tacs' -> tac :: intros_induction tacs'
  | [] -> []   

(* Returns true if the given tactic solves the goal. *)
let solves env sigma (tac : unit Proofview.tactic) (goal : constr) =
  let p = Proof.start sigma [(env, EConstr.of_constr goal)] in
  let (p', _) = Proof.run_tactic (Global.env()) tac p in
  let (result, _, _, _, _) = Proof.proof p' in
  List.length result == 0

(* Given the list of tactics and their corresponding string
   expressions, try to solve the goal (type of trm),
   return None otherwise. *)
let try_solve env sigma opts trm =
  try 
    let goal = (Typeops.infer env trm).uj_type in
    let rec aux opts =
      match opts with
      | [] -> None
      | (tac, expr) :: opts' ->
         if solves env sigma tac goal
         then Some (Expr expr)
         else aux opts'
    in aux opts
  with _ -> None
          
(* Performs the bulk of decompilation on a proof term.
   Opts are the optional goal solving tactics that can be inserted into
     the generated script. If at any point one of these tactics solves the
     remaining goal, use the provided string representation of that tactic.
   Returns a list of tactics. *)
let rec first_pass env sigma (opts : (unit Proofview.tactic * string) list) trm =
  (* Apply single reduction to terms that *might*
       be in eta expanded form. *)
  let trm = Reduction.whd_betaiota env trm in
  let solved = try_solve env sigma opts trm in
  if Option.has_some solved then [Option.get solved]
  else 
    let choose f x =
      Option.default [Apply (env, trm)] (f x (env, sigma, opts)) in
    match kind trm with
    (* "fun x => ..." -> "intro x." *)
    | Lambda (n, t, b) ->
       let (env', trm', names) = zoom_lambda_names env 0 trm in
       Intros names :: first_pass env' sigma opts trm'
    (* Match on well-known functions used in the proof. *)
    | App (f, args) ->
       choose (rewrite <|> induction <|> left <|> right <|> split
               <|> reflexivity <|> symmetry <|> exists) (f, args)
    (* Hypothesis transformations or generation tactics. *)
    | LetIn (n, valu, typ, body) ->
       choose (rewrite_in <|> apply_in <|> pose) (n, valu, typ, body)
    (* Remainder of body, simply apply it. *)
    | _ -> [ Apply (env, trm) ]
         
(* Application of a equality eliminator. *)
and rewrite (f, args) (env, sigma, opts) : tact list option =
  dest_rewrite (mkApp (f, args)) >>= fun rewr -> 
  Some (Rewrite (env, rewr.eq, rewr.left) :: first_pass env sigma opts rewr.px)

(* Applying an eliminator for induction on a hypothesis in context. *)
and induction (f, args) (env, sigma, opts) : tact list option =
  guard (is_elim env f) >>= fun _ ->
  guard (not (is_rewrite f)) >>= fun _ ->
  let app = mkApp (f, args) in
  let sigma, ind = deconstruct_eliminator env sigma app in
  let ind_args = ind.final_args in
  inductive_of_elim env (destConst f) >>= fun from_i ->
  let from_m = lookup_mind from_i env in
  let ari = arity (type_of_inductive env 0 from_m) in
  let ind_pos = ari - List.length ind.pms in
  if ind_pos >= List.length ind.final_args
  then
    (Feedback.msg_error (str "Failed to decompile induction");
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
    let goals = List.map (fun (env, trm, _) ->
                    simpl (first_pass env sigma opts trm)) zooms in
    let (prefix, goals) = tact_shared_prefix goals in  
    let ind = [ Induction (env, ind_var, names, prefix, goals) ] in
    Some (if reverts == [] then ind else Revert reverts :: ind)
    
(* Choose left proof to construct or. *)
and left (f, args) (env, sigma, opts) : tact list option =
  dest_or_introl (mkApp (f, args)) >>= fun args ->
  Some (Left :: first_pass env sigma opts args.ltrm)

(* Choose right proof to construct or. *)
and right (f, args) (env, sigma, opts) : tact list option =
  dest_or_intror (mkApp (f, args)) >>= fun args ->
  Some (Right :: first_pass env sigma opts args.rtrm)

(* Branch two goals as arguments to conj. *)
and split (f, args) (env, sigma, opts) : tact list option =
  dest_conj (mkApp (f, args)) >>= fun args ->
  let lhs = first_pass env sigma opts args.ltrm in
  let rhs = first_pass env sigma opts args.rtrm in
  let (prefix, goals) = tact_shared_prefix [ lhs ; rhs ] in
  Some [ Split (prefix, goals) ]

(* Converts "apply eq_refl." into "reflexivity." *)
and reflexivity (f, args) _ : tact list option =
  dest_eq_refl_opt (mkApp (f, args)) >>= fun _ ->
  Some [ Reflexivity ]
  
(* Transform x = y to y = x. *)
and symmetry (f, args) (env, sigma, opts) : tact list option =
  guard (equal f eq_sym) >>= fun _ ->
  let sym = dest_eq_sym (mkApp (f, args)) in
  Some (Symmetry :: first_pass env sigma opts sym.eq_proof)

(* Provide evidence for dependent pair.  *)
and exists (f, args) (env, sigma, opts) : tact list option =
  guard (equal f Sigmautils.existT) >>= fun _ ->
  let exT = Sigmautils.dest_existT (mkApp (f, args)) in
  Some (Exists (env, exT.index) :: first_pass env sigma opts exT.unpacked)
  
(* Value must be a rewrite on a hypothesis in context. *)
and rewrite_in (_, valu, _, body) (env, sigma, opts) : tact list option =
  let valu = Reduction.whd_betaiota env valu in
  try_app valu                   >>= fun (f, args) ->
  dest_rewrite (mkApp (f, args)) >>= fun rewr -> 
  try_rel rewr.px                >>= fun idx ->
  guard (noccurn (idx + 1) body) >>= fun _ ->
  let n, t = rel_name_type (lookup_rel idx env) in
  let env' = push_local (n, t) env in
  Some (RewriteIn (env, rewr.eq, rewr.px, rewr.left) ::
          first_pass env' sigma opts body)

(* Value must be an application with last argument in context. *)
and apply_in (n, valu, typ, body) (env, sigma, opts) : tact list option =
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
    let args' = List.map (first_pass env' sigma opts) (Array.to_list args) in
    Some (ApplyIn (env, prf, hyp) :: List.concat ((first_pass env' sigma opts f) :: args'))
  in 
  (* all other cases *)
  let default app_in (_, sigma) = Some (ApplyIn (env, prf, hyp)
                                        :: first_pass env' sigma opts body)
  in
  (apply_binding <|> default) () (env', sigma)
    
(* Last resort decompile let-in as a pose.  *)
and pose (n, valu, t, body) (env, sigma, opts) : tact list option =
  let n' = fresh_name env n in
  let env' = push_let_in (Name n', valu, t) env in
  let decomp_body = first_pass env' sigma opts body in
  (* If the binding is NEVER used, just skip this. *)
  if noccurn 1 body then Some decomp_body
  else Some (Pose (env, valu, n') :: decomp_body)
       
(* Decompile a term into its equivalent tactic list. *)
let tac_from_term env sigma opts trm : tact list =
  (* Perform second pass to revise greedy tactic list. *)
  (intros_revert (first_pass env sigma opts trm))

(* Generate indentation space before bullet. *)
let indent level =
  let spacing level = (level - 2) / 3 + 2 in
  let rec aux i = if i <= 1 then 0
                  else spacing i + aux (i - 1)
  in str (String.make (aux level) ' ')

(* Make bullets in order of: -, +, *, --, ++, **, ---, etc. *)
let bullet level =
  let num = (level + 2) / 3 in
  let blt = match level mod 3 with
    | 0 -> '*'
    | 1 -> '-'
    | 2 -> '+' in
  str (String.make num blt) ++ str " "
  
(* Indented string representation of a tactic list. *)
let rec show_tact_list level sigma (tacs : tact list) : Pp.t =
  let f i = show_tact (i == 0 && level > 0) level sigma (str ".\n") in
  seq (List.mapi f tacs)

(* List of tactics on same line separated by semicolons. *)
and show_prefix_list sigma (tacs : tact list) : Pp.t =
  if tacs == []
  then str ".\n"
  else str "; " ++
    let last_idx = List.length tacs - 1 in
    let f i =
      let fin = if i == last_idx then str ".\n" else str "; " in
      show_tact false 0 sigma fin in
    seq (List.mapi f tacs)

(* Show prefix list followed by indented subgoals. *)
and show_subgoals level sigma prefix goals =
  show_prefix_list sigma prefix ++
    seq (List.map (show_tact_list (level + 1) sigma) goals)
    
(* Return the string representation of a single tactic. *)
and show_tact (bulletted : bool) level sigma (fin : Pp.t) tac : Pp.t =
  let prnt e = Printer.pr_constr_env e sigma in
  let full_indent = if bulletted
                    then indent level ++ bullet level
                    else indent (level + 1) in
  full_indent ++
    (match tac with
     | Intros ns ->
        if ns == [] then str "" else
          let s = if List.tl ns == [] then "intro" else "intros" in
          let names = String.concat " " (List.map Id.to_string ns) in
          str (s ^ " " ^ names) ++ fin
     | Apply (env, trm) ->
        str "apply " ++ prnt env trm ++ fin
     | Rewrite (env, trm, left) ->
        let s = prnt env trm in
        let arrow = if left then "<- " else "" in
        str ("rewrite " ^ arrow) ++ s ++ fin
     | RewriteIn (env, prf, hyp, left) ->
        let prf_s, hyp_s = prnt env prf, prnt env hyp in
        let arrow = if left then "" else "<- " in
        str ("rewrite " ^ arrow) ++ prf_s ++ str " in " ++ hyp_s ++ fin
     | ApplyIn (env, prf, hyp) ->
        let prf_s, hyp_s = prnt env prf, prnt env hyp in
        str "apply " ++ prf_s ++ str " in " ++ hyp_s ++ fin
     | Pose (env, hyp, n) ->
        let n = str (Id.to_string n) in
        str "pose " ++ prnt env hyp ++ str " as " ++ n ++ fin
     | Induction (env, trm, names, prefix, goals) ->
        let to_s ns = if ns == [] then " " (* prevent "||" *)
                      else String.concat " " (List.map Id.to_string ns) in
        let bindings = str (String.concat "|" (List.map to_s names)) in
        str "induction " ++ prnt env trm ++
          str " as [" ++ bindings ++ str "]" ++
          show_subgoals level sigma prefix goals
     | Reflexivity -> str "reflexivity" ++ fin 
     | Simpl -> str "simpl" ++ fin
     | Left -> str "left" ++ fin
     | Right -> str "right" ++ fin
     | Split (prefix, goals) ->
        str "split" ++ show_subgoals level sigma prefix goals
     | Revert ns ->
        if ns == [] then str "" else
          let names = String.concat " " (List.rev_map Id.to_string ns) in
          str ("revert " ^ names) ++ fin
     | Symmetry -> str "symmetry" ++ fin
     | Exists (env, trm) ->
        str "exists " ++ prnt env trm ++ fin
     | Auto -> str "auto" ++ fin
     | Expr s -> str s ++ fin)
    
(* Represent tactics as a string. *)
let tac_to_string = show_tact_list 0 
  