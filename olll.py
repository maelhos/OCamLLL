from sage.all import matrix
from fractions import Fraction
from typing import List, Sequence


class Vector(list):
    def __init__(self, x):
        super().__init__(map(Fraction, x))

    def sdot(self) -> Fraction:
        return self.dot(self)

    def dot(self, rhs: "Vector") -> Fraction:
        """
        >>> Vector([1, 2, 3]).dot([4, 5, 6])
        Fraction(32, 1)
        """
        rhs = Vector(rhs)
        assert len(self) == len(rhs)
        return sum(map(lambda x: x[0] * x[1], zip(self, rhs)))

    def proj_coff(self, rhs: "Vector") -> Fraction:
        """
        >>> Vector([1, 1, 1]).proj_coff([-1, 0, 2])
        Fraction(1, 3)
        """
        rhs = Vector(rhs)
        assert len(self) == len(rhs)
        return self.dot(rhs) / self.sdot()

    def proj(self, rhs: "Vector") -> "Vector":
        """
        >>> Vector([1, 1, 1]).proj([-1, 0, 2])
        [1/3, 1/3, 1/3]
        """
        rhs = Vector(rhs)
        assert len(self) == len(rhs)
        return self.proj_coff(rhs) * self

    def __sub__(self, rhs: "Vector") -> "Vector":
        """
        >>> Vector([1, 2, 3]) - [6, 5, 4]
        [-5, -3, -1]
        """
        rhs = Vector(rhs)
        assert len(self) == len(rhs)
        return Vector(x - y for x, y in zip(self, rhs))

    def __mul__(self, rhs: Fraction) -> "Vector":
        """
        >>> Vector(["3/2", "4/5", "1/4"]) * 2
        [3, 8/5, 1/2]
        """
        return Vector(x * rhs for x in self)

    def __rmul__(self, lhs: Fraction) -> "Vector":
        """
        >>> 2 * Vector(["3/2", "4/5", "1/4"])
        [3, 8/5, 1/2]
        """
        return Vector(x * lhs for x in self)

    def __repr__(self) -> str:
        return "[{}]".format(", ".join(str(x) for x in self))


def gramschmidt(v: Sequence[Vector]) -> Sequence[Vector]:
    """
    >>> gramschmidt([[3, 1], [2, 2]])
    [[3, 1], [-2/5, 6/5]]
    >>> gramschmidt([[4, 1, 2], [4, 7, 2], [3, 1, 7]])
    [[4, 1, 2], [-8/7, 40/7, -4/7], [-11/5, 0, 22/5]]
    """
    u: List[Vector] = []
    for vi in v:
        ui = Vector(vi)
        for uj in u:
            ui = ui - uj.proj(vi)

        if any(ui):
            u.append(ui)
    return u


def reduction(basis: Sequence[Sequence[int]], delta: float) -> Sequence[Sequence[int]]:
    n = len(basis)
    basis = list(map(Vector, basis))
    ortho = gramschmidt(basis)

    def mu(i: int, j: int) -> Fraction:
        return ortho[j].proj_coff(basis[i])

    k = 1
    while k < n:
        print("nb :", basis[:k+1])
        print("no :", ortho[:k+1])
        print()
        for j in range(k - 1, -1, -1):
            mu_kj = mu(k, j)
            if abs(mu_kj) > 0.5:
                basis[k] = basis[k] - basis[j] * round(mu_kj)
                ortho = gramschmidt(basis)

        if ortho[k].sdot() >= (delta - mu(k, k - 1)**2) * ortho[k - 1].sdot():
            k += 1
        else:
            basis[k], basis[k - 1] = basis[k - 1], basis[k]
            ortho = gramschmidt(basis)
            k = max(k - 1, 1)

    return [list(map(int, b)) for b in basis]

def reduction_rec(basis: List[List[int]], delta: float) -> List[List[int]]:
    n = len(basis)
    basis = list(map(Vector, basis))
    ortho = gramschmidt(basis)

    k = 1
    new_basis = basis[:2]
    new_ortho = basis[:2]

    zip_basis = basis[2:][::-1]
    zip_ortho = basis[2:][::-1]

    def mu(i: int, j: int) -> Fraction:
        return new_ortho[j].proj_coff(new_basis[i])
    
    while k < n:
        print("nb :", new_basis)
        print("no :", new_ortho)
        print()
        for j in range(k - 1, -1, -1):
            mu_kj = mu(k, j)
            if abs(mu_kj) > 0.5:
                new_basis[-1] -= new_basis[j] * round(mu_kj)
                new_ortho = gramschmidt(new_basis)

        if new_ortho[-1].sdot() >= (delta - mu(k, k - 1)**2) * new_ortho[-2].sdot():
            k += 1
            if k < n:
                new_basis.append(zip_basis.pop())
                new_ortho.append(zip_ortho.pop())
        else:
            new_basis[-1], new_basis[-2] = new_basis[-2], new_basis[-1]
            new_ortho = gramschmidt(new_basis)
            if k != 1:
                zip_basis.append(new_basis.pop())
                zip_ortho.append(new_ortho.pop())

            k = max(k - 1, 1)

    return [list(map(int, b)) for b in new_basis]

# test 

if __name__ == "__main__":
    print(reduction([[4.0, 1.0, 3.0,-1.0], 
                     [2.0, 1.0,-3.0, 4.0], 
                     [1.0, 0.0,-2.0, 7.0], 
                     [6.0, 2.0, 9.0,-5.0]], 
                     1))
    
    print("##############")
    print(reduction_rec([[4.0, 1.0, 3.0,-1.0], 
                         [2.0, 1.0,-3.0, 4.0], 
                         [1.0, 0.0,-2.0, 7.0], 
                         [6.0, 2.0, 9.0,-5.0]], 
                         1))
    
    print("##############")
    print(list(matrix([[4, 1, 3,-1], 
                         [2, 1,-3, 4], 
                         [1, 0,-2, 7], 
                         [6, 2, 9,-5]]).LLL()))