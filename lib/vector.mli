module type Field = sig 
	type t

	val zero : t 
	val one : t

	val add : t -> t -> t 
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val neg : t -> t
	val compare : t -> t -> int
end


module type MakeVector = functor (F : Field) ->
sig
	type t = F.t array

	val (+) : t -> t -> t
	val (-) : t -> t -> t 
	val (<.>) : t -> t -> F.t
	val (~-) : t -> t
	val ( * ) : F.t -> t -> t
	val norm : t -> F.t
	val ( <<>> ) : t -> t -> F.t
	val ( </> ) : t -> t -> t
end

module MakeVector : MakeVector

val pp_vector : (Format.formatter -> 'a -> unit) -> 'a array -> unit
(* pretty printing for vectors. *)