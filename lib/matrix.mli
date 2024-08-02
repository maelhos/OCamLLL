module type MakeMatrix = functor (F : Vector.Field) ->
sig
	type mt = F.t array list

	val gram_schmidt : mt -> mt
	val pp_matrix : sep:char -> (Format.formatter -> F.t -> unit) -> mt -> unit
	val lll : ?delta:F.t -> mt -> mt
end

module MakeMatrix : MakeMatrix