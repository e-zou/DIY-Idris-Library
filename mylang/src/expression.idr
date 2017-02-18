module expression

import nat
import variable
import state
import uint8

--------------------------------
-- SYNTAX for Nat expressions --
--------------------------------

||| NatExpr is the type of expressions that evaluate to
||| natural numbers (Nat). We'll call them Nat Expressions.
public export
data NatExpr =
  NatLitExpr Nat |
  NatVarExpr (Variable Nat) |
  NatPlusExpr NatExpr NatExpr |
  NatMinusExpr NatExpr NatExpr |
  NatMultExpr NatExpr NatExpr

-----------------------------------
-- SEMANTICS for Nat Expressions --
-----------------------------------

||| natExprEval evaluates Nat Expressions to yield Nats.
export
natExprEval: NatExpr -> State -> Nat
natExprEval (NatLitExpr n) st = n
natExprEval (NatVarExpr v) st = (getNatState st) v
natExprEval (NatPlusExpr e1 e2) st =
  nat_plus   -- the result is the sum of
    (natExprEval e1 st) -- the value of expr1
    (natExprEval e2 st) -- and expr2
natExprEval (NatMinusExpr e1 e2) st =
    nat_sub
    (natExprEval e1 st)
    (natExprEval e2 st)
natExprEval (NatMultExpr e1 e2) st =
  nat_mult
    (natExprEval e1 st)
    (natExprEval e2 st)

----------------------------------
-- SYNTAX for UInt8 expressions --
----------------------------------

||| UInt8Expr is the type of expressions that evaluate to
||| unassigned byte8 integers. We'll call them UInt8 Expressions.
public export
data UInt8Expr =
  UInt8LitExpr UInt8 |
  UInt8VarExpr (Variable UInt8) |
  UInt8PlusExpr UInt8Expr UInt8Expr |
  UInt8IncExpr UInt8Expr |
  UInt8DecExpr UInt8Expr

-------------------------------------
-- SEMANTICS for UInt8 Expressions --
-------------------------------------

||| UInt8ExprEval evaluates UInt8 Expressions to yield UInt8s.
export
uInt8ExprEval: UInt8Expr -> State -> UInt8
uInt8ExprEval (UInt8LitExpr u) st = u
uInt8ExprEval (UInt8VarExpr v) st = (getUInt8State st) v
uInt8ExprEval (UInt8PlusExpr e1 e2) st =
  uint8_plus
    (uInt8ExprEval e1 st)
    (uInt8ExprEval e2 st)
uInt8ExprEval (UInt8IncExpr e1) st =
  uint8_inc (uInt8ExprEval e1 st)
uInt8ExprEval (UInt8DecExpr e1) st =
  uint8_dec (uInt8ExprEval e1 st)

---------------------------------
-- Syntax for Bool expressions --
---------------------------------

||| BoolExpr is the type of expressions that evaluate to
||| Boolean values (Bool). We'll call them Bool Expressions.
public export
data BoolExpr =
  BoolLitExpr Bool |
  BoolVarExpr (Variable Bool) |
  BoolAndExpr BoolExpr BoolExpr |
  BoolEqExpr NatExpr NatExpr |
  BoolNeqExpr NatExpr NatExpr |
  BoolEqExprUInt8 UInt8Expr UInt8Expr

------------------------------------
-- Semantics for Bool expressions --
------------------------------------

||| natExprEval evaluates Nat Expressions to yield Nats.
export
boolExprEval: BoolExpr -> State -> Bool
boolExprEval (BoolLitExpr b) st = b
boolExprEval (BoolVarExpr v) st = (getBoolState st) v
boolExprEval (BoolAndExpr e1 e2) st =
    bool_and (boolExprEval e1 st) (boolExprEval e2 st)
boolExprEval (BoolEqExpr n1 n2) st =
    eq (natExprEval n1 st) (natExprEval n2 st)
boolExprEval (BoolNeqExpr n1 n2) st =
    neq (natExprEval n1 st) (natExprEval n2 st)
boolExprEval (BoolEqExprUInt8 u1 u2) st =
    eq (uInt8ExprEval u1 st) (uInt8ExprEval u2 st)
