open Vector

module type MakeMatrix = functor (F : Vector.Field) ->
  sig
    type mt = F.t array list
  
    val gram_schmidt : mt -> mt 
    val pp_matrix : sep:char -> (Format.formatter -> F.t -> unit) -> mt -> unit
  end

module MakeMatrix (F : Field) =
struct
  open MakeVector(F)
  type mt = t list

  let gram_schmidt (m : mt) : mt =
    let rec step (vec_done : mt) (m : mt) : mt =
      match m with 
      | [] -> vec_done 
      | vi :: rest -> begin
        let new_vec = List.fold_left (fun acc uj -> acc - (vi </> uj)) vi vec_done in 
        step (new_vec :: vec_done) rest
      end
    in m |> step [] |> List.rev

  let lll (m : mt) ?(delta = F.one) : mt =
    let n = List.length m in
    let rec aux (basis : mt) (ortho : mt) : mt * mt =
      begin
      let mus = [] in
      let mukkm1 = List.hd mus in
      if (F.compare (norm new_ortho) (F.mul (F.mul mukkm1 mukkm1 |> F.sub delta) (ortho |> List.hd |> norm)) < 0) then
        begin
        let new_basis = List.hd basis :: new_vec :: List.tl basis in 
        let new_ortho = List.hd ortho :: new_ortho :: List.tl ortho in
        match new_basis with
        | [_, _] -> aux new_basis new_ortho
        | _ -> aux (List.tl new_basis) (List.tl new_ortho)
        end
      else
        aux (new_vec :: basis) (new_ortho :: ortho)
      end
    in 
    fst (aux m (gram_schmidt m))
  
  let pp_matrix ~sep (fmt : Format.formatter -> F.t -> unit) (m : mt) : unit =
        List.iter (fun v -> begin
          Format.print_string "[";
          let v_len = Array.length v in
          Array.iteri (fun i a -> Format.printf "%a" fmt a; if i < (Int.sub v_len 1) then Format.print_char sep else ()) v;
          Format.print_string "]\n"
        end) m
end
