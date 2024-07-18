module type MakeMatrix = functor (F : Vector.Field) ->
sig
	type mt = F.t array list

	val gram_schmidt : mt -> mt
	val pp_matrix : sep:char -> (Format.formatter -> F.t -> unit) -> mt -> unit
end

module MakeMatrix : MakeMatrix