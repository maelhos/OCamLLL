open! OCamLLL.Matrix

module MatrixFloat = MakeMatrix(Float)

let () =
  let m = [
    [| 4. ; 1. ; 3. ; -1. |] ;
    [| 2. ; 1. ; -3. ; 4. |] ;
    [| 1. ; 0. ; -2. ; 7. |] ;
    [| 6. ; 2. ; 9. ; -5. |]
  ] in 
  let res = MatrixFloat.gram_schmidt (m :> MatrixFloat.mt)
  in print_newline (); MatrixFloat.pp_matrix Format.pp_print_float (List.rev res)


module MatrixQ = MakeMatrix(Q)

let () =
  let m = [
    [| 4 ; 1 ; 3 ; -1 |] ;
    [| 2 ; 1 ; -3 ; 4 |] ;
    [| 1 ; 0 ; -2 ; 7 |] ;
    [| 6 ; 2 ; 9 ; -5 |]
  ] in 
  let m_c = List.map (fun a -> Array.map (fun b -> Q.of_int b) a) m in
  let res = MatrixQ.gram_schmidt (m_c :> MatrixQ.mt)
  in print_newline (); MatrixQ.pp_matrix Q.pp_print (List.rev res)