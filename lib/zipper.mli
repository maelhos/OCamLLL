exception Impossible
(* 
	We make assumptions over our zipper, therefore :
	our right and left lists cannot be empty
*)

module type Zipper = sig 
	type 'a t = 'a list * 'a list

	val empty : 'a t 

	val is_empty : 'a t -> bool 

	val to_list : 'a t -> 'a list 

	val to_rev_list : 'a t -> 'a list 

	val make : 'a list -> 'a t 

	val left : 'a t -> 'a t 

   val right : 'a t -> 'a t 

	val modify : ('a -> 'a) -> 'a t -> 'a t 

	val insert : 'a -> 'a t -> 'a t

	val remove : 'a t -> 'a t 

	val focused : 'a t -> 'a

	val drop_before : 'a t -> 'a t 

	val drop_after : 'a t -> 'a t

	val swap_heads : 'a t -> 'a t 

	val get_left : 'a t -> 'a list 
  
	val get_right : 'a t -> 'a list
end

module Zipper : Zipper