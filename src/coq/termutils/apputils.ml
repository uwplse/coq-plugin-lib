(*
 * Utilities for applications of terms
 *)

open EConstr
open Utilities

(* --- Constructing applications --- *)

(* mkApp with a list *)
let mkAppl (f, args) = mkApp (f, Array.of_list args)

(* --- Deconstructing applications --- *)

(* Get a list of all arguments, fully unfolded at the head *)
let unfold_args_app sigma trm =
  let (f, args) = destApp sigma trm in
  let rec unfold trm =
    match kind sigma trm with
    | App (f, args) ->
       List.append (unfold f) (Array.to_list args)
    | _ ->
       [trm]
  in List.append (List.tl (unfold f)) (Array.to_list args)

(* Like unfold_args_app, but return empty if it's not an application *)
let unfold_args sigma trm =
  if isApp sigma trm then unfold_args_app sigma trm else []

(* Get the last argument of an application *)
let last_arg sigma trm =
  if isApp sigma trm then last (unfold_args sigma trm) else failwith "not an application"

(* Get the first function of an application *)
let rec first_fun sigma t =
  match kind sigma t with
  | App (f, args) ->
     first_fun sigma f
  | _ ->
     t

(*
 * Get the argument to an application of a property at argument position i
 * This unfolds all arguments first
 *)
let get_arg sigma i trm =
  match kind sigma trm with
  | App (_, _) ->
     let args = Array.of_list (unfold_args sigma trm) in
     Array.get args i
  | _ ->
     failwith "not an application"

(* --- Questions about application --- *)

(* Check whether trm applies f (using equal for equality) *)
let applies sigma (f : types) (trm : types) =
  match kind sigma trm with
  | App (g, _) ->
     eq_constr sigma f g
  | _ ->
     false

(* Plural version *)
let apply sigma (trm : types) = and_p (applies sigma trm)

(* Check whether trm is trm' or applies trm', using equal *)
let is_or_applies sigma (trm' : types) (trm : types) : bool =
  applies sigma trm' trm || eq_constr sigma trm' trm

(* Plural version *)
let are_or_apply sigma (trm : types) = and_p (is_or_applies sigma trm)
                            
(*
 * Is the first term equal to a "head" (application prefix) of the second?
 * The notion of term equality is syntactic (i.e., no environment) and defaults
 * to syntactic equality modulo alpha, casts, grouping, and universes. The
 * result of this function is an informative boolean: an optional array, with
 * None meaning false and Some meaning true and giving the trailing arguments.
 *
 * This function is similar to is_or_applies, except for term equality and the
 * informative boolean result.
 *)
let eq_constr_head ?(eq_constr=eq_constr_nounivs) sigma term term' =
  let head, args = decompose_app sigma term in
  let head', args' = decompose_app sigma term' in
  if eq_constr sigma head head' && Util.List.prefix_of (eq_constr sigma) args args' then
    Some (Util.List.skipn (Util.List.length args) args' |> Array.of_list)
  else
    None
