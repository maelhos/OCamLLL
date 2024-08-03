open! OCamLLL.Matrix

module MatrixFloat = MakeMatrix(Float)

let () =
  let m = [
    [| 1. ; 1. ; 1. |] ;
    [| -1. ; 0. ; 2. |] ;
    [| 3. ; 5. ; 6. |]
  ] in 
  let res = MatrixFloat.lll ?delta:(Some 0.75) (m :> MatrixFloat.mt)
  in print_newline (); MatrixFloat.pp_matrix ~sep:' ' Format.pp_print_float res


module MatrixQ = MakeMatrix(Q)

let () =
  let m = [
    [| 105 ; 821 ; 404 ; 328 |] ;
    [| 881 ; 667 ; 644 ; 927 |] ;
    [| 181 ; 483 ; 87 ; 500 |] ;
    [| 893 ; 834 ; 732 ; 441 |]
  ] in 
  let m_c = List.map (fun a -> Array.map (fun b -> Q.of_int b) a) m in
  let res = MatrixQ.lll (m_c :> MatrixQ.mt)
  in print_newline (); MatrixQ.pp_matrix ~sep:' ' Q.pp_print res