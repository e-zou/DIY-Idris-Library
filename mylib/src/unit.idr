||| Abstract data type simulating algebra with one value and identity function
module unit

import bool
import eq

||| The type with only one value
export
data Unit =
  ||| single term of type Unit
  MkUnit

||| Hides the data type and constructor
||| Give a non-constructor name to the only value of this type
export
void: Unit
void = MkUnit
-- hides MkUnit and clients refer unit_id as "void"

||| The identity function or type Unit
export
unit_id : Unit -> Unit
unit_id MkUnit = MkUnit
-- other files can't use MkUnit if we used unit.idr in other files

||| Takes two units and return true if they are the same
||| and return false if they are not
export
unit_eq: Unit -> Unit -> Bool
unit_eq u1 u2 = True

||| Implementation of Eq interface for Unit
export
implementation Eq Unit where
  eq b1 b2 = unit_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)

{-
no export: no one else can use it outside of this file
export: client can use module but can't see constructors or functions
public export: client can see module, usable constructors, and
underlying funcitons
constructors: things enabling you to build values of another type
data type: defines set of values
  - indirect method to see the unit type
-}
