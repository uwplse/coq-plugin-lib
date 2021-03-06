(*
 * Basic utilities for collections, optionals, and so on
 *)

(* --- Optionals --- *)

(*
 * Map a function on an optional, and return a default value if it's none
 * This should be in the standard library, but for some reason locally is not
 *)
val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b

(* Monadic guard for option. *)
val guard : bool -> unit option

(* --- Lists --- *)

val last : 'a list -> 'a
val all_but_last : 'a list -> 'a list
val snoc : 'a -> 'a list -> 'a list
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

(*
 * Take n elements of a list
 *)
val take : int -> 'a list -> 'a list

(*
 * Drop first n elements from a list.
 * Throws exception if n exceeds the length of the list.
 *)
val drop : int -> 'a list -> 'a list

(*
 * Take all but n elements of a list
 *)
val take_except : int -> 'a list -> 'a list

(*
 * Split a list l into (l1, l2) where |l1| = n and |l2| = |l| - n
 *)
val take_split : int -> 'a list -> ('a list * 'a list)

(*
 * Remove duplicates from a list
 *)
val unique : ('a -> 'a -> bool) -> 'a list -> 'a list
                                             
(*
 * Map a function over a list, then flatten the result
 *)
val flat_map : ('a -> 'b list) -> 'a list -> 'b list

(*
 * Map elements of a list to optionals, then filter out Nones.
 *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  
(*
 * Return true if a list has length > 0
 *)
val non_empty : 'a list -> bool

(*
 * Return the offset of an element that satisfies p in a
 * Fail if the element is not in the list
 *)
val find_off : 'a list -> ('a -> bool) -> int 

(*
 * All combinations of elements in a list
 *)
val combinations : 'a list -> ('a * 'a) list

(*
 * Cartesian product of two lists
 *)
val cartesian : 'a list -> 'b list -> ('a * 'b) list
                                                
(*
 * Combine all permutations of pairs of elements in two lists
 * Use some combinator function to combine them
 *)
val combine_cartesian : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(*
 * Turns an array of lists [[t11, t12] [t21, t22] ..] into a list
 * of arrays [[t11 t21 ..] .. [t11 t22 ..] .. [t12 t21 ..] .. [t12 t22 ..] ..]
 *)
val combine_cartesian_append : 'a list array -> 'a array list
                                                                     
(*
 * [min, max)
 *)
val range : int -> int -> int list

(*
 * [1, max]
 *)
val from_one_to : int -> int list

(*
 * Splits a list at an index into two lists
 *)
val split_at : int -> 'a list -> (('a list) * ('a list))

(* 
 * Try to get the second element of a list, defaulting
 * to the first, raising NotFound if empty. 
 *)
val list_snd : 'a list -> 'a
  
(* 
 *Compare whether all elements of two lists of equal length are equal. 
 *)
val list_eq : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
          
(*
 * Compare if all elements of a single list are equal.
 *)
val all_eq : ('a -> 'a -> bool) -> 'a list ->  bool

(* 
 * Count length of shared prefix between lists.
 *)
val count_shared_prefix : ('a -> 'a -> bool) -> 'a list -> 'a list -> int
  
(* --- Tuples --- *)

val map_tuple : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)
val fold_tuple : ('a -> 'b -> 'c) -> ('a * 'b) -> 'c
val twice : ('a -> 'a -> bool -> 'b) -> 'a -> 'a -> ('b * 'b)
val reverse: ('a * 'b) -> ('b * 'a)
val pair : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)

(* --- Propositions --- *)

val always_true : 'a -> bool          
val and_p : ('a -> bool) -> 'a -> 'a -> bool

(* --- Control structures --- *)

val map_if_else : ('a -> 'b) -> ('a -> 'b) -> bool -> 'a -> 'b
val map_if : ('a -> 'a) -> bool -> 'a -> 'a

(* --- Functions --- *)

(*
 * Flip the first and second parameters of a function.
 *)
val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

(* --- Common helper functions --- *)
                                                   
(*
 * The identity function
 *)
val id : 'a -> 'a

(*
 * Get a fresh constant identifier
 *)
val fid : unit -> int
