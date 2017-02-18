module variableTest

import variable
import nat
import uint8

-- A few test cases for var_eq (refactor)

public export
X: Variable Nat
X = variable_new nat_zero

public export
Y: Variable Nat
Y = variable_new nat_one

public export
Z: Variable Nat
Z = variable_new nat_two

-- Now we can also declare variable that
-- to which we will assign (associate) Bool values

A: Variable Bool
A = variable_new nat_zero

B: Variable Bool
B = variable_new nat_one

C: Variable Bool
C = variable_new nat_two


-- Tests of bool_eq

b1: Bool
b1 = var_eq X X

b2: Bool
b2 = var_eq X Y


-- Tests of eq (for Bool)

b3: Bool
b3 = eq X Y

b4: Bool
b4 = eq Y Y

-- Tests for Unassigned integers
public export
U: Variable UInt8
U = variable_new nat_zero

U1: Variable UInt8
U1 = variable_new nat_one

U2: Variable UInt8
U2 = variable_new nat_two

u1: Bool
u1 = var_eq U U1

u2: Bool
u2 = var_eq U U
