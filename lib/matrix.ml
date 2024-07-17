module type MakeMatrix = functor (F : Vector.Field) ->
sig
  type t = Vector.MakeVector(F).t array
end

module MakeMatrix (F : Vector.Field) =
struct
  type t = Vector.MakeVector(F).t array
end