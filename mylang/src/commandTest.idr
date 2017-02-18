module commandTest

import command
import nat
import uint8
import variableTest   -- defines variables X, Y, Z
import variable
import expression
import state

-------------------------------------------
---- putting together a program for factorial of n
-------------------------------------------

a: State
a = state_init

{-
In Python, the code looks like this:
x = 5
y = 1
while x != 0:
    y = y * x
    x = x - 1
-}

-- X = 5
xGetsFive1: Command
xGetsFive1 = NatAssign
            X
            (NatLitExpr nat_five)


a': State
a' = CommandEval xGetsFive1 a


-- Y = 0
yGetsOne: Command
yGetsOne = NatAssign
            Y
            (NatLitExpr nat_one)


a'': State
a'' = CommandEval yGetsOne a'


-- X != 0
Xneq0: BoolExpr
Xneq0 = BoolNeqExpr
            (NatVarExpr X)
            (NatLitExpr nat_zero)



-- Y = Y * X
accumXinY1: Command
accumXinY1 = NatAssign
        Y
        (NatMultExpr
            (NatVarExpr Y)
            (NatVarExpr X))

a''': State
a''' = CommandEval accumXinY1 a''
-- X = X - 1
decrX1: Command
decrX1 = NatAssign
        X
        (NatMinusExpr (
            NatVarExpr X)
            (NatLitExpr nat_one))
-- acculumate x in y decrementing x until x = 0
iterateSum1: Command
iterateSum1 = While
                Xneq0
                (Seq
                    accumXinY1
                    decrX1)
-- initialize X = 5, then run the loop
factOneToFive: Command
factOneToFive =
        Seq
        xGetsFive1
        (Seq
            yGetsOne
            iterateSum1)
-- now (st X) = 0 and (st Y) = result
--------------------------------------
---- now we run (evaulate) our program
--------------------------------------
-- the answer will be in Y in the resulting state
a'''': State
a'''' = CommandEval factOneToFive state_init
-- Here's a version of the whole program all in one expression
allInOne_factOneToFive: Command
allInOne_factOneToFive =
    Seq
    (NatAssign X (NatLitExpr nat_five))
    (Seq
        (NatAssign Y (NatLitExpr nat_one))
        (While
            (BoolNeqExpr (NatVarExpr X) (NatLitExpr nat_zero))
            (Seq
                (NatAssign Y (NatMultExpr (NatVarExpr Y) (NatVarExpr X)))
                (NatAssign X (NatMinusExpr (NatVarExpr X) (NatLitExpr nat_one))))))

-- Tests: Programs are just values of this data type!

-------------------------------------------
---- putting together a program for sum-1-X
-------------------------------------------

st: State
st = state_init

{-
In Python, the code looks like this:
x = 5
y = 0
while x != 0:
    y = y + x
    x = x - 1
-}

-- X = 5
xGetsFive: Command
xGetsFive = NatAssign
            X
            (NatLitExpr (nat_succ (nat_succ nat_three)))


st': State
st' = CommandEval xGetsFive st


-- Y = 0
yGetsZero: Command
yGetsZero = NatAssign
            Y
            (NatLitExpr nat_zero)


st'': State
st'' = CommandEval yGetsZero st'


-- X != 0
Xnot0: BoolExpr
Xnot0 = BoolNeqExpr
            (NatVarExpr X)
            (NatLitExpr nat_zero)



-- Y = Y + X
accumXinY: Command
accumXinY = NatAssign
        Y
        (NatPlusExpr
            (NatVarExpr Y)
            (NatVarExpr X))

st''': State
st''' = CommandEval accumXinY st''
-- X = X - 1
decrX: Command
decrX = NatAssign
        X
        (NatMinusExpr (
            NatVarExpr X)
            (NatLitExpr nat_one))
-- acculumate x in y decrementing x until x = 0
iterateSum: Command
iterateSum = While
                Xnot0
                (Seq
                    accumXinY
                    decrX)
-- initialize X = 5, then run the loop
sumOneToFive: Command
sumOneToFive =
        Seq
        xGetsFive
        (Seq
            yGetsZero
            iterateSum)
-- now (st X) = 0 and (st Y) = result
--------------------------------------
---- now we run (evaulate) our program
--------------------------------------
-- the answer will be in Y in the resulting state
st'''': State
st'''' = CommandEval sumOneToFive state_init
-- Here's a version of the whole program all in one expression
allInOne_sumOneToFive: Command
allInOne_sumOneToFive =
    Seq
    (NatAssign X (NatLitExpr (nat_succ (nat_succ nat_three))))
    (Seq
        (NatAssign Y (NatLitExpr nat_zero))
        (While
            (BoolNeqExpr (NatVarExpr X) (NatLitExpr nat_zero))
            (Seq
                (NatAssign Y (NatPlusExpr (NatVarExpr Y) (NatVarExpr X)))
                (NatAssign X (NatMinusExpr (NatVarExpr X) (NatLitExpr nat_one))))))

st''''': State
st''''' = CommandEval allInOne_sumOneToFive state_init
-----------------------------------------------------
-------- separate examples of other commands --------
-----------------------------------------------------

-- Example of ...
-- if X != 0:
--     X = 5
condCmd: Command
condCmd = IfThenElse
            Xnot0
            xGetsFive   -- improve this kjs
            Skip   -- improve this kjs



------------------------------------
------------ older stuff -----------
------------------------------------
-- Tests: Programs are just values of this data type!

-- Do nothing but skip
prog0: Command
prog0 = Skip

{-
s0: State
s0 = run prog0 state_init
-}

-- A single assignment
prog1: Command
prog1 = NatAssign X (NatLitExpr nat_two)

{-
s1: State
s1 = run prog1 state_init
-- expect s1 X = 2, s1 Y = 0, s1 Z = 0
-- An assignment in the state produced by the last assignment
s2: State
s2 = run (Assign Y nat_two) s1
-- expect s2 X = 2, s2 Y = 2, s2 Z = 0
-}

-- A sequential composition of assignments
prog2: Command
prog2 =
  Seq
    (NatAssign X (NatLitExpr nat_one))
    (NatAssign Y (NatLitExpr nat_two))

{-
s3: State
s3 = run prog2 state_init
-- expect same result as s2
-}

-- A sequential composition of an assignment with a larger program
prog3: Command
prog3 =
  Seq
    (NatAssign X (NatLitExpr nat_one))
    (Seq
      (NatAssign Y (NatLitExpr nat_two))
      (NatAssign Z (NatLitExpr nat_three)))

{-
s4: State
s4 = run prog3 state_init
-}

-- NEW TEST CASES USING EXPRESSIONS

-- like "X = Y" in Python
prog4: Command
prog4 = NatAssign X (NatVarExpr Y)

-- like "X = Y + 1" in Python
prog5: Command
prog5 =
  NatAssign
    X
    (NatPlusExpr
      (NatVarExpr Y)
      (NatLitExpr nat_one))

---------------------------------
----- Test Cases with UInt8 -----
---------------------------------
state1: State
state1 = state_init

cmd1: Command
cmd1 = UInt8Assign U
          (UInt8LitExpr uint8_one)

state2: State
state2 = CommandEval cmd1 state1

-- like "U = U + 1" in Python
cmd2: Command
cmd2 =
  UInt8Assign
    U
    (UInt8PlusExpr
      (UInt8VarExpr U)
      (UInt8LitExpr uint8_one))

state3: State
state3 = CommandEval cmd2 state2

cmd3: Command
cmd3 =
  UInt8Assign
    U
      (UInt8IncExpr (UInt8LitExpr uint8_one))

state4: State
state4 = CommandEval cmd3 state3

cmd4: Command
cmd4 =
  UInt8Assign
    U
      (UInt8DecExpr (UInt8LitExpr uint8_one))

state5: State
state5 = CommandEval cmd4 state4

-- Test for BoolEqExprUInt8
v1: Variable Bool
v1 = variable_new nat_zero

command1: Command
command1 = BoolAssign v1 (BoolEqExprUInt8 (UInt8LitExpr uint8_one) (UInt8LitExpr uint8_one))

command1state: State
command1state = CommandEval command1 state_init


{-
In Python, the code looks like this:
x = byte8_5
y = byte8_0
while x != byte8_zero:
    y = y + x
    x = x - 1
    -}

X1: Variable UInt8
X1 = variable_new nat_zero

Y1: Variable UInt8
Y1 = variable_new nat_one

byteSumOneToFive: Command
byteSumOneToFive =
   Seq
    (UInt8Assign X1 (UInt8LitExpr (uint8_five)))
    (Seq
        (UInt8Assign Y1 (UInt8LitExpr uint8_zero))
        (While
            (BoolEqExprUInt8 (UInt8VarExpr X1) (UInt8LitExpr uint8_zero))
            (Seq
                (UInt8Assign Y1 (UInt8PlusExpr (UInt8VarExpr Y1) (UInt8VarExpr X1)))
                (UInt8Assign X1 (UInt8DecExpr (UInt8VarExpr X1))))))

st1: State
st1 = CommandEval byteSumOneToFive state_init
