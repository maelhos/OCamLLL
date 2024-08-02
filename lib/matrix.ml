open Vector
open Zipper

module type MakeMatrix = functor (F : Vector.Field) ->
  sig
    type mt = F.t array list
  
    val gram_schmidt : mt -> mt 
    val pp_matrix : sep:char -> (Format.formatter -> F.t -> unit) -> mt -> unit
    val lll : ?delta:F.t -> mt -> mt
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

  let create_mus ortho vec_basis =
    List.map (fun v -> v <<>> vec_basis) ortho

  let rec create_new_basis mus basis new_basis_vec =
    (* In this function, we should have |mus| = |basis| *)
    match mus, basis with 
    | _, [] -> new_basis_vec
    | mu_kj :: rest_mus, curr_vec :: rest_vecs ->
      create_new_basis rest_mus rest_vecs (new_basis_vec - mu_kj * curr_vec)
    | _ -> raise Impossible

  let lll ?(delta = F.one) (basis : mt) : mt =
    let _n = List.length basis in
    let basis_zipper = Zipper.make basis in
    let ortho_zipper = Zipper.make @@ gram_schmidt basis in
    let rec reduce (basis : t Zipper.t) (ortho : t Zipper.t) : t Zipper.t =
      let mus = create_mus (Zipper.get_left ortho) (basis |> Zipper.get_left |> List.hd) in
      let mukkm1 = List.hd mus in
      let new_basis_vec = create_new_basis mus (Zipper.get_left basis) (Zipper.focused basis) in
      let new_ortho_vec = Zipper.get_left basis |> gram_schmidt |> List.hd in
      let basis_with_vec = Zipper.insert new_basis_vec basis in
      if F.(compare (norm new_ortho_vec) (mul (mul mukkm1 mukkm1 |> sub delta) (ortho |> Zipper.focused |> norm)) < 0) then
        begin
        let new_basis = Zipper.swap_heads basis_with_vec in 
        let new_ortho = (gram_schmidt (Zipper.get_left new_basis), Zipper.get_right new_basis) in
        match Zipper.get_left new_basis with
        | [_] ->
          reduce new_basis new_ortho
        | _ ->
          reduce (Zipper.right new_basis) (Zipper.right new_ortho)
        end
      else
        let new_basis = Zipper.right basis in 
        let new_ortho = Zipper.right ortho in 
        reduce new_basis new_ortho
    in 
    reduce basis_zipper ortho_zipper |> Zipper.to_list
  
  let pp_matrix ~sep (fmt : Format.formatter -> F.t -> unit) (m : mt) : unit =
        List.iter (fun v -> begin
          Format.print_string "[";
          let v_len = Array.length v in
          Array.iteri (fun i a -> Format.printf "%a" fmt a; if i < (Int.sub v_len 1) then Format.print_char sep else ()) v;
          Format.print_string "]\n"
        end) m
end
