open Vector
open Zipper

module type MakeMatrix = functor (F : Vector.Field) ->
  sig
    type mt = F.t array list

    val half : F.t
  
    val gram_schmidt : mt -> mt 
    val pp_matrix : sep:char -> (Format.formatter -> F.t -> unit) -> mt -> unit
    val lll : ?delta:F.t -> mt -> mt
  end

module MakeMatrix (F : Field) =
struct
  open MakeVector(F)
  type mt = t list

  let half = F.(div one @@ add one one)

  let gram_schmidt (m : mt) : mt =
    let rec step (vec_done : mt) (m : mt) : mt =
      match m with 
      | [] -> vec_done 
      | vi :: rest -> begin
        let new_vec = List.fold_left (fun acc uj -> acc - (vi </> uj)) vi vec_done in 
        step (new_vec :: vec_done) rest
      end
    in m |> step [] |> List.rev

  let create_mus ortho vec_basis =
    List.map (fun v -> v <<>> vec_basis) ortho

  let rec create_new_basis mus basis new_basis_vec =
    (* In this function, we should have |mus| = |basis| *)
    match mus, basis with 
    | _, [] -> [new_basis_vec]
    | mu_kj :: rest_mus, curr_vec :: rest_vecs ->
      begin
        if F.(compare (abs mu_kj) half < 0) then
          let modified_basis_vec = new_basis_vec - mu_kj * curr_vec in
          modified_basis_vec :: (create_new_basis rest_mus rest_vecs modified_basis_vec)
        else
          new_basis_vec :: (create_new_basis rest_mus rest_vecs new_basis_vec)
      end
    | _ -> raise Impossible

  let pp_matrix ~sep (fmt : Format.formatter -> F.t -> unit) (m : mt) : unit =
    List.iter (fun v -> begin
      Format.print_string "[";
      let v_len = Array.length v in
      Array.iteri (fun i a -> Format.printf "%a" fmt a; if i < (Int.sub v_len 1) then Format.print_char sep else ()) v;
      Format.print_string "]\n"
    end) m

  let swap_top l =
    match l with 
    | [] | [_] -> l 
    | x1 :: x2 :: rest -> x2 :: x1 :: rest

  let lll ?(delta = F.one) (basis : mt) : mt =
    let basis_zipper = basis |> Zipper.make |> Zipper.right in
    let ortho_zipper = basis |> gram_schmidt |> Zipper.make |> Zipper.right in
    let rec reduce (basis : t Zipper.t) (ortho : t Zipper.t) : t Zipper.t =
      begin
      let () = pp_matrix ~sep:' ' (fun fmt x -> Format.pp_print_string fmt F.(to_string x)) (Zipper.to_list basis) in 
      let () = pp_matrix ~sep:' ' (fun fmt x -> Format.pp_print_string fmt F.(to_string x)) (Zipper.to_list ortho) in
      let () = print_newline () in
      let mus = create_mus (ortho |> Zipper.get_left |> List.tl) (Zipper.focused basis) in
      let mukkm1 = List.hd mus in
      let new_basis = create_new_basis mus (basis |> Zipper.get_left |> List.tl) (Zipper.focused basis) in
      let new_ortho = gram_schmidt new_basis in
      let new_basis_vec = List.hd new_basis in 
      let new_ortho_vec = List.hd new_ortho in
      let basis_with_vec = basis |> Zipper.remove |> Zipper.insert new_basis_vec in
      if F.(compare (norm new_ortho_vec) (mul (mul mukkm1 mukkm1 |> sub delta) (ortho |> Zipper.focused |> norm)) < 0) then
        begin
        let new_basis = (basis_with_vec |> Zipper.get_left |> swap_top, Zipper.get_right basis_with_vec) in 
        let new_ortho = (new_basis |> Zipper.get_left |> gram_schmidt, Zipper.get_right new_basis) in
        match Zipper.get_left new_basis with
        | [_] ->
          reduce new_basis new_ortho
        | _ ->
          reduce (Zipper.right new_basis) (Zipper.right new_ortho)
        end
      else
        try
          let new_basis = Zipper.right (new_basis, Zipper.get_right basis) in 
          let new_ortho = Zipper.right (new_ortho, Zipper.get_right ortho) in 
          reduce new_basis new_ortho
        with 
        | Impossible -> (new_basis, Zipper.get_right basis)
      end
    in 
    reduce basis_zipper ortho_zipper |> Zipper.to_list
end
