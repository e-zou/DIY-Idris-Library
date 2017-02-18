||| Abstract data type simulating Boolean algebra
module bool


||| A data type of which represent Boolean true and false
public export
data Bool =
  ||| Term representing the concept Boolean truth
  True |
  ||| Term representing the concept of Boolean falsehood
  False


||| Representation of identity function on Boolean values
{-Capitalization matters. Capitalize for data types.-}
export
bool_id: Bool -> Bool
bool_id b = b
{-
bool_id  True = True;
bool_id  False = False;
-}

||| Representation of constant true functions in Boolean values
export
bool_true: Bool -> Bool
bool_true _ = True
{-
bool_true True = True;
bool_true False = True;
-}

||| Representation of constant false functions in Boolean values
export
bool_false: Bool -> Bool
bool_false _ = False
{-
bool_false True = False;
bool_fase False = False;
-}

||| Representation of negation function in Boolean values
export
bool_not: Bool -> Bool
bool_not True = False
bool_not _ = True
{-
bool_not False = True;
-}

||| Representation of Boolean "and" function
export
bool_and: Bool -> Bool -> Bool
bool_and True True = True
bool_and _ _ = False
{-
bool_and True False False
bool_and False True False
bool_and False False False
-}

||| Representation of Boolean "or" function
export
bool_or: Bool -> Bool -> Bool
bool_or True True = True
bool_or True False = True
bool_or False True = True
bool_or False False = False

||| Representation of Boolean "nand" function
export
bool_nand: Bool -> Bool -> Bool
bool_nand b1 b2 = bool_not (bool_and b1 b2)
{-
bool_nand True False = False
boo_nand False True = False
bool_nand False False = False
or
bool_nand: Bool -> Bool -> Bool
bool_nand True True = False
bool_nand _ _ = True
-}

||| Representation of Boolean "xor" function
export
bool_xor : Bool -> Bool -> Bool
bool_xor True False = True
bool_xor False True = True
bool_xor _ _ = False
{-
bool_xor True True = False
bool_xor False False = False
-}

||| Representation of Boolean "implies" function
export
bool_implies : Bool -> Bool -> Bool
bool_implies False _ = True
bool_implies True True = True
bool_implies True _ = False
{-
bool_implies False True = True
bool_implies False False = True
bool_implies True True = True
bool_implies True False = False
-}

||| Representation of Boolean equiv function
export
bool_equiv : Bool -> Bool -> Bool
bool_equiv True True = True
bool_equiv False False = True
bool_equiv _ _ = False
{-
bool_equiv True False = False
bool_equiv False True = False
-}

export
bool_eq: Bool -> Bool -> Bool
bool_eq b1 b2 = bool_equiv b1 b2

||| Representation of Boolean if-then-else
export
bool_if_then_else: Bool -> Bool -> Bool -> Bool
{-ITE statement-}
{-bool_if_then_else True False True = False
bool_if_then_else False False True = True-}
bool_if_then_else True t _ = t {-return the first value-}
bool_if_then_else False _ f = f {-return the second value-}
{-if the first condition is true, return the first result;
if the first condition is False, return the second result-}
