module expressionTest

import state
import variable
import expression
import nat
import bool
import uint8

V: Variable Bool
V = variable_new nat_zero

--- We'll call the initial state, st
-- State is now a pair of functions - one for bool, one for nat
st: State
st = state_init

--- override: result function like input but maps V to True
st': State
st' = state_override_bool st V True

--- a Nat Variable with a nice name
N: Variable Nat
N = variable_new nat_zero

--- Now create state with V = True and N = nat_one.
st'': State
st'' = state_override_nat st' N nat_one

-- third Nat Variable is now assigned the value nat_two
st''': State
st''' = state_override_nat st'' N nat_two

--- a UInt8 Variable with a name
U: Variable UInt8
U = variable_new nat_one

--- state with V=True, N = nat_two, and U = uint8_one
st'''': State
st'''' = state_override_uint8 st''' U uint8_one
