||| An abstract data type, nat, simulating
||| the natural numbers and common arithmetic
||| operations involving natural numbers
module list_nat

import nat
import ifthenelse
import bool
import eq

||| The primary data type we export is Nat.
||| The constructors are hidden from other modules.
export
data ListNat =
  ||| Nil_Nat represents the empty list of natural numbers
  NilNat |
  ||| Con_Nat head tail represents the
  ConsNat Nat ListNat

||| Return the Nat representing zero
export
list_nat_empty: ListNat
list_nat_empty = NilNat

||| Identity function for list_nat_cons
export
list_nat_id: ListNat -> ListNat
list_nat_id l = l

||| Return the (Nat representing the) successor of the given Nat
export
list_nat_cons: Nat -> ListNat -> ListNat
list_nat_cons n l = ConsNat n l

||| Compute the tail of a given list (with tail nil = nil)
export
list_nat_tail: ListNat -> ListNat
list_nat_tail NilNat = NilNat
list_nat_tail (ConsNat head tail) = tail

||| (This function simulates) natural number addition
||| in Peano arithmetic
export
list_nat_plus: ListNat -> ListNat -> ListNat
list_nat_plus NilNat m = m
list_nat_plus (ConsNat h t) m =
  ConsNat h (list_nat_plus t m)
