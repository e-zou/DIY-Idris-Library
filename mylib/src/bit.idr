module bit
import bool
import ifthenelse

public export
data Bit = MkBit Bool
{-what is MkBit and what is it's purpose?-}

public export
B0: Bit
B0 = MkBit False

public export
B1: Bit
B1 = MkBit True

||| Bit
export
bit_new: Bool -> Bit
bit_new b = MkBit b


export
bit_and: Bit -> Bit -> Bit
bit_and (MkBit b1) (MkBit b2) = MkBit (bool_and b1 b2)

export
bit_not: Bit -> Bit
bit_not (MkBit b1) = MkBit (bool_not b1)

export
bit_or: Bit -> Bit -> Bit
bit_or (MkBit b1) (MkBit b2) = MkBit (bool_or b1 b2)

||| Bit equality
export
bit_eq: Bit -> Bit -> Bool
bit_eq (MkBit bool1) (MkBit bool2) =
    bool_eq bool1 bool2

public export
bit_rep: Bit -> Bool
bit_rep (MkBit b) = b

public export
bit_plus: Bit -> Bit -> Bit
bit_plus (MkBit b1) (MkBit b2) = MkBit (bool_xor b1 b2)
{-this is a called a "sum bit" when given two bits. If both argument bits are
zero, the result is the zero bit; if one bit is zero and the other is one, the
result is one; and if both bits are one, the result is zero. We saw that if we
represent bits as Boolean values, we can represent bit addition as exclusive or
applied to Boolean values.-}

public export
bit_carry: Bit -> Bit -> Bit
bit_carry (MkBit b1) (MkBit b2) = MkBit (bool_and b1 b2)
{-This is a "carry bit". This calculates which number to carry over when doing
binary arithmetic.-}

||| Compute the sum bit of three bits
export
bit_plus3: Bit -> Bit -> Bit -> Bit
bit_plus3 (MkBit b1) (MkBit b2) (MkBit cin) =
  MkBit (bool_xor (bool_xor b1 b2) cin)

||| Compute the carry bit of three bits
export
bit_carry3: Bit -> Bit -> Bit -> Bit
bit_carry3 (MkBit b1) (MkBit b2) (MkBit cin) =
  MkBit (bool_or
          (bool_and (bool_xor b1 b2) cin)
          (bool_and b1 b2))

||| Base cases for bit subtraction
export
bit_sub: Bit -> Bit -> Bit
bit_sub (MkBit b1) (MkBit b2) = MkBit (bool_xor b1 b2)
