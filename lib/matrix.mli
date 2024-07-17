open Vector

module type MakeMatrix = functor (F : Field) ->
sig
	type t = MakeVector(F).t array
end