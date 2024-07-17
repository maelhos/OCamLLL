module type Field = sig 
	type t

	val zero : t 
	val one : t

	val add : t -> t -> t 
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val neg : t -> t
end

module type MakeVector = functor (F : Field) ->
sig
	type t = F.t array

	val (+) : t -> t -> t
	val (-) : t -> t -> t 
	val (<.>) : t -> t -> F.t
	val (~-) : t -> t
	val ( * ) : F.t -> t -> t
  val ( </> ) : t -> t -> t
end

module MakeVector (F : Field) =
struct 
  type t = F.t array

  let (+) (v1 : t) (v2 : t) : t =
    Array.map2 F.add v1 v2
  
  let (-) (v1 : t) (v2 : t) : t =
    Array.map2 F.sub v1 v2 
  
  let (<.>) (v1 : t) (v2 : t) : F.t =
    let sum = v1 + v2 in 
    Array.fold_left F.add F.zero sum
  
  let (~-) (v1 : t) : t =
    Array.map F.neg v1
  
  let ( * ) (x : F.t) (v : t) =
    Array.map (F.mul x) v

  let ( </> ) (v : t) (u : t) : t =
    let lambda = F.div (v <.> u) (u <.> u) in
    lambda * u
end

let pp_vector fmt v =
	Array.iter (fun x -> Format.printf "[ %a ]\n" fmt x) v