open! OCamLLL.Matrix

module Matrix = MakeMatrix(Stdlib.Float)

let () =
  let m = [
    [| 4. ; 1. ; 3. ; -1. |] ;
    [| 2. ; 1. ; -3. ; 4. |] ;
    [| 1. ; 0. ; -2. ; 7. |] ;
    [| 6. ; 2. ; 9. ; -5. |]
  ] in 
  let res = Matrix.gram_schmidt (m :> Matrix.mt)
  in print_newline (); Matrix.pp_matrix Format.pp_print_float (List.rev res)