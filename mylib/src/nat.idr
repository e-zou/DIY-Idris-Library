||| Abstract data type, nat, simulating the natural numbers and common
||| arithmetic operations involving natural numbers
module nat

-- Some arithmetic funcitons return Boolean values; so clients will need bool
import public bool
import eq

||| The primary data type we export is Nat
||| The constructors themselves are private.
export
data Nat =
  ||| Z constructs a representation of zero
  Z |
  ||| S constructs a representation of the successor
  ||| of the given natural number (which is zero in this case)
  S Nat


||| The Nat representing zero
export
nat_zero: Nat
nat_zero = Z

||| The Nat representing one
export
nat_one: Nat
nat_one = S Z

||| The Nat representing two
export
nat_two: Nat
nat_two = S nat_one
-- (S(S Z))

||| The Nat representing three
export
nat_three: Nat
nat_three = S nat_two


||| The Nat identity function
export
nat_id: Nat -> Nat
nat_id n = n

||| Representing the successor Nat
export
nat_succ: Nat -> Nat
nat_succ n = (S n)

export
nat_four: Nat
nat_four = nat_succ nat_three

export
nat_five: Nat
nat_five = nat_succ nat_four

export
nat_six: Nat
nat_six = nat_succ nat_five

export
nat_seven: Nat
nat_seven = nat_succ nat_six

export
nat_eight: Nat
nat_eight = nat_succ nat_seven

export
nat_nine: Nat
nat_nine = nat_succ nat_eight

export
nat_ten: Nat
nat_ten = nat_succ nat_nine

||| Representing the predecessor Nat
export
nat_pred: Nat -> Nat
nat_pred Z = Z
nat_pred (S n) = n

||| Representing the addition of two natural numbers
{-This is a recursive function.-}
export
nat_plus: Nat -> Nat -> Nat
nat_plus Z n = n
nat_plus (S m) n = S (nat_plus m n)

||| Representing the subtraction of two natural numbers
export
nat_sub: Nat -> Nat -> Nat
nat_sub m Z = m
nat_sub Z (S n) = Z
nat_sub (S m) (S n) = (nat_sub m n)

||| Assigning even natural numbers to the boolean value of True
export
nat_even: Nat -> Bool
nat_even Z = True
nat_even (S Z) = False
nat_even (S n) = (bool_id (nat_even (nat_pred n)))

||| Assigning even natural numbers to the boolean value of False
export
nat_odd: Nat -> Bool
nat_odd Z = False
nat_odd (S Z) = True
nat_odd (S n) = (bool_not (nat_even (nat_pred n)))

||| Compute the product of two nats
export
nat_mult: Nat -> Nat -> Nat
nat_mult Z m = Z
nat_mult (S n) m = nat_plus m (nat_mult n m)


||| Return the factorial of a given nat
||| Recall: if n = 0, n! = 1 and if n > 0, n! = n * (n-1)!
export
nat_fact: Nat -> Nat
nat_fact Z = nat_succ nat_zero
nat_fact (S n) = nat_mult (S n) (nat_fact n)

||| Returns number equality
export
nat_eq: Nat -> Nat -> Bool
nat_eq Z Z = True
nat_eq Z (S n) = False
nat_eq (S n) Z = False
nat_eq (S m) (S n) = nat_eq m n


-- And some code to test it all out

nat4: Nat
nat4 = nat_plus Z Z

nat5: Nat
nat5 = nat_plus Z (S (S Z))

nat6: Nat
nat6 = nat_plus (S (S Z)) Z

nat7: Nat
nat7 = nat_plus (S (S Z)) (S (S Z))

||| Return true if the first natural number argument
||| is less than or equal to the second argument n <= m
export
nat_le: Nat -> Nat -> Bool
nat_le Z Z = True
nat_le Z (S n) = True
nat_le (S n) Z = False
nat_le (S n) (S m) = nat_le n m

||| Return true if the first natural number, n, is
||| strictly less than the second, m (i.e., n < m).
-- Hint: n < m if n <= m and n is not equal to m.
export
nat_lt: Nat -> Nat -> Bool
nat_lt n m = bool_and (nat_le n m) (bool_not (nat_eq n m))

||| Returns true if n > m, and otherwise false (where n and m are Nats)
export
nat_gt: Nat -> Nat -> Bool
nat_gt n m = bool_not (nat_le n m)

||| Returns true if n >= m and false otherwise
export
nat_ge: Nat -> Nat -> Bool
nat_ge n m = bool_not (nat_lt n m)

||| Return n raised to the power m. Recall that any
||| natural number to the zero power is 1, and that
||| a natural number, n, to the m'th power (n > 0) is
||| n times n to the (m-1) power. You will have to
||| replace the single rule in the following code
||| with rules for base and recursive cases.
export
nat_pow: Nat -> Nat -> Nat
nat_pow n Z = nat_one
nat_pow n (S m) = nat_mult n (nat_pow n m)

||| Returns a natural number squared
export
nat_square: Nat -> Nat
nat_square n = nat_pow n nat_two

||| Implementation of Eq interface for Bool
export
implementation Eq Nat where
  eq b1 b2 = nat_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)

||| Applies a function to a natural number and return another natural number
export
apply1: (Nat -> Nat) -> Nat -> Nat
apply1 func n = func n

||| Applies a function to two natural numbers
export
apply2: (Nat -> Nat -> Nat) -> Nat -> Nat -> Nat
apply2 func n m = func n m

export
apply3: (Nat -> Nat) -> (Nat -> Nat) -> Nat -> Nat
apply3 func1 func2 n = func1 (func2 n)
