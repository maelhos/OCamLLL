open Vector

module type MakeMatrix = functor (F : Vector.Field) ->
  sig
    type mt = F.t array list
  
    val gram_schmidt : mt -> mt 
    val pp_matrix : (Format.formatter -> F.t -> unit) -> mt -> unit
  end

module MakeMatrix (F : Field) =
struct
  open MakeVector(F)
  type mt = t list

  let gram_schmidt (m : mt) : mt =
    let rec step (m : mt) (vec_done : mt) : mt =
      match m with 
      | [] -> vec_done 
      | vi :: rest -> begin
        let new_vec = List.fold_left (fun acc uj -> acc - (uj </> vi)) vi vec_done in 
        step rest (new_vec :: vec_done)
      end
    in step m [] |> List.rev
  
  let pp_matrix (fmt : Format.formatter -> F.t -> unit) (m : mt) : unit =
        List.iter (fun v -> begin
          Format.print_string "[";
          let v_len = Array.length v in
          Array.iteri (fun i a -> Format.printf "%a" fmt a; if i < (Int.sub v_len 1) then Format.print_char ' ' else ()) v;
          Format.print_string "]\n"
        end) m
end