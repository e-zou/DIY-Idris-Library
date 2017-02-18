module listTest

import list
import list_nat

-- []: List Nat
-- []
||| Smallest list
ln: List Nat
ln = Nil

-- Cons 1 []: ListNat
-- [1]
||| List of natural numbers starting with one
ln1: List Nat
ln1 = Cons nat_one ln

-- Cons 2 (Cons 1 []): ListNat
-- [2, 1]
ln2: List Nat
ln2 = Cons nat_two ln1

-- Cons 3 (Cons 2 (Cons 1 [])): ListNat
-- [3, 2, 1]
ln3: List Nat
ln3 = Cons nat_three ln2

lb4: List Bool
lb4 = Cons (True) (Cons False (Cons (True) (Cons False Nil)))
