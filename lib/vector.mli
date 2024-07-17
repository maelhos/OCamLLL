module type Field = sig 
	type t

	val zero : t 
	val one : t

	val (+) : t -> t -> t 
	val (-) : t -> t -> t
	val ( * ) : t -> t -> t
	val ( / ) : t -> t -> t
	val (~-) : t -> t 
	val (<) : t -> t -> bool 
	val (>) : t -> t -> bool 
	val (<=) : t -> t -> bool 
	val (>=) : t -> t -> bool 
	val (<>) : t -> t -> bool 
	val (=) : t -> t -> bool
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

module MakeVector : MakeVector