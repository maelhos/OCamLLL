# OCamLLL

Pure OCaml implementation of LLL algorithm.
As a reminder, the Lenstra-Lenstra-Lovász (LLL) is a lattice base reduction algorithm in polynomial time.
Given a base $\mathcal{B}=\{b_1,...,b_d\}$, with $n$-dimensional integer coordinates, for a lattice $L$ (discrete subgroup of $\mathbb{R}^n$), with $d\le n$, it calculates a nearly orthogonal lattice base in time $\mathcal{O}(d^5n\log^3B)$, with $B$ being the largest length of $b_i$ over the Euclidian norm.

# Usage

This project has been created using `dune`, hence to build it you have to use the command :
```shell
dune build
```
And then to execute its main function (in `bin/main.ml`), you can execute :
```shell
dune exec OCamLLL
```

If you want to use our library for your own code, you can find in `lib` the most important files : we redefine vectors and matrix using functors `MakeVector` (`lib/vector.ml`) and `MakeMatrix` (`lib/matrix.ml`), and then we implement Gram-Schmidt algorithm and LLL in their corresponding `ml` files.
If you want to test our library on real use cases, you can check `test/*` and run :
```shell
dune runtest
```

# Requirements

We use `zarith` library for our field type and the rest is in the `lib` directory. You can install it running :
```shell
opam install zarith
```