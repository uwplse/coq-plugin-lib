(*
 * Utilities for applications of terms
 *)

open Constr
open Utilities

(* --- Constructing applications --- *)

(* mkApp with a list *)
let mkAppl (f, args) = mkApp (f, Array.of_list args)

(* --- Deconstructing applications --- *)

(* Get a list of all arguments, fully unfolded at the head *)
let unfold_args_app trm =
  let (f, args) = destApp trm in
  let rec unfold trm =
    match kind trm with
    | App (f, args) ->
       List.append (unfold f) (Array.to_list args)
    | _ ->
       [trm]
  in List.append (List.tl (unfold f)) (Array.to_list args)

(* Like unfold_args_app, but return empty if it's not an application *)
let unfold_args trm =
  if isApp trm then unfold_args_app trm else []

(* Get the last argument of an application *)
let last_arg trm =
  if isApp trm then last (unfold_args trm) else failwith "not an application"

(* Get the first function of an application *)
let rec first_fun t =
  match kind t with
  | App (f, args) ->
     first_fun f
  | _ ->
     t

(*
 * Get the argument to an application of a property at argument position i
 * This unfolds all arguments first
 *)
let get_arg i trm =
  match kind trm with
  | App (_, _) ->
     let args = Array.of_list (unfold_args trm) in
     Array.get args i
  | _ ->
     failwith "not an application"

(* --- Questions about application --- *)

(* Check whether trm applies f (using equal for equality) *)
let applies (f : types) (trm : types) =
  match kind trm with
  | App (g, _) ->
     equal f g
  | _ ->
     false

(* Plural version *)
let apply (trm : types) = and_p (applies trm)

(* Check whether trm is trm' or applies trm', using equal *)
let is_or_applies (trm' : types) (trm : types) : bool =
  applies trm' trm || equal trm' trm

(* Plural version *)
let are_or_apply (trm : types) = and_p (is_or_applies trm)
                            
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
let eq_constr_head ?(eq_constr=eq_constr_nounivs) term term' =
  let head, args = decompose_app term in
  let head', args' = decompose_app term' in
  if eq_constr head head' && Util.List.prefix_of eq_constr args args' then
    Some (Util.List.skipn (Util.List.length args) args' |> Array.of_list)
  else
    None
