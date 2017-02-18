||| A library of functions for binary arithmetic
module binary

import bool
import bit
import byte2

||| Compute the summ bit of two bits
public export
bit_plus: Bit -> Bit -> Bit
bit_plus (MkBit b1) (MkBit b2) = MkBit (bool_xor b1 b2)
{-this is a called a "sum bit" when given two bits. If both argument bits are
zero, the result is the zero bit; if one bit is zero and the other is one, the
result is one; and if both bits are one, the result is zero. We saw that if we
represent bits as Boolean values, we can represent bit addition as exclusive or
applied to Boolean values.-}

||| Computes the carry bit of two bits
public export
bit_carry: Bit -> Bit -> Bit
bit_carry (MkBit b1) (MkBit b2) = MkBit (bool_and b1 b2)
{-This is a "carry bit". This calculates which number to carry over when doing
binary arithmetic.-}

||| Implementation of a half_adder (returns byte that combines 2 bits.)
public export
half_adder: Bit -> Bit -> Byte2
half_adder b1 b2 = MkByte2 (bit_carry b1 b2) (bit_plus b1 b2)
{-
same as:
public export
half_adder: Bit -> Bit -> Byte 2
half_adder b1 b0 =
   MykByte2
      (bit_carry b1 b0)
      (bit_plus b1 b0)
-}
