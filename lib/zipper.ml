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

module Zipper = struct 
  type 'a t = 'a list * 'a list

  let empty = ([], [])

  let is_empty (l1, l2) = (l1 = []) && (l2 = [])

  let to_list (l1, l2) =
    List.rev_append l1 l2 

  let to_rev_list (l1, l2) =
    List.rev_append l2 l1 

  let make l =
    ([List.hd l], List.tl l)

  let left (l1, l2) =
    match l1 with 
    | [] -> (l1, l2)
    | curr :: rest -> (rest, curr::l2)

  let right (l1, l2) = 
    match l2 with 
    | [] | [_] -> (l1, l2)
    | curr :: rest -> (curr :: l1, rest)

  let modify f (l1, l2) =
    match l1 with 
    | to_modify :: rest -> (f to_modify :: rest, l2)
    | [] -> (l1, l2)

  let insert x (l1, l2) =
    (x :: l1, l2)

  let remove (l1, l2) =
    match l1, l2 with 
    | [], [] -> ([], [])
    | [], _ :: rest -> ([], rest)
    | _ :: rest, _ -> (rest, l2)
  
  let focused (l1, _) =
    match l1 with 
    | [] -> raise Impossible
    | curr :: _ -> curr 

  let drop_before (_, l2) =
    ([], l2)

  let drop_after (l1, _) =
    (l1, [])

  let swap_heads (l1, l2) =
    match l1, l2 with 
    | [], _ | _, [] -> raise Impossible
    | curr1 :: rest1, curr2 :: rest2 ->
      let new_left = curr2 :: rest1 in 
      let new_right = curr1 :: rest2 in
      (new_left, new_right)

  let get_left (l1, _) =
    l1

  let get_right (_, l2) =
    l2
end