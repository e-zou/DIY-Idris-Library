module fib

import nat
import ifthenelse

{-
n = int(input())
fib(n):
  while n > 0:
    if n == 0:
      return 0
    elif n == 1:
      return 1
    else:
      return fib (n - 1) + fib (n - 2)
-}

fib: Nat -> Nat
fib nat =
  ifthenelse
  (nat_eq nat nat_zero)
  (nat_zero)
    (ifthenelse
      (nat_eq nat nat_one)
      (nat_one)
      (nat_plus (fib (nat_pred nat)) (fib (nat_pred (nat_pred (nat))))))

t0: Nat
t0 = fib nat_zero
-- 0

t1: Nat
t1 = fib nat_one
-- 1

t2: Nat
t2 = fib nat_two
-- 1

t3: Nat
t3 = fib nat_three
-- 2

t4: Nat
t4 = fib nat_four
-- 3

t5: Nat
t5 = fib nat_five
-- 5

t6: Nat
t6 = fib nat_six
