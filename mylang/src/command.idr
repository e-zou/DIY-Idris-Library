module command

import variable
import nat
import uint8
import expression
import state
import ifthenelse

-----------------------------------------------------
----------------- SYNTAX ----------------------------
-----------------------------------------------------

-- The syntax of a little imperative programming language!
public export
data Command =
  Skip |
  NatAssign (Variable Nat) NatExpr |
  BoolAssign (Variable Bool) BoolExpr |
  UInt8Assign (Variable UInt8) UInt8Expr |
  Seq Command Command |
  IfThenElse BoolExpr Command Command |
  While BoolExpr Command


-----------------------------------------------------
--------------- OPERATIONAL SEMANTICS! --------------
-----------------------------------------------------

export
CommandEval: Command -> State -> State
CommandEval Skip st = st
CommandEval (NatAssign v e) st =
  state_override_nat st v (natExprEval e st)
CommandEval (BoolAssign v e) st =
  state_override_bool st v (boolExprEval e st)
CommandEval (UInt8Assign v e) st =
  state_override_uint8 st v (uInt8ExprEval e st)
CommandEval (Seq c1 c2) st =
  CommandEval c2 (CommandEval c1 st)
CommandEval (IfThenElse cond tcmd fcmd) st =
  ifthenelse
    (boolExprEval cond st)
    (CommandEval tcmd st)
    (CommandEval fcmd st)
CommandEval (While cond cmd) st =
  ifthenelse
    (boolExprEval cond st)
    (CommandEval (While cond cmd) (CommandEval cmd st))
    (CommandEval Skip st)
