||| Abstract data type representing two-bit byte
module byte2

{-Can't use inline documentation here because there is no Definition of things
using the same file.-}
import public bit
{-We add "public" to import bit module to tell Idris to export them to
accordingly users of the byte module. Users of byte modules can see bits. We
want clients to know that bytes are made from bits.-}
import bool
import eq

||| A two-bit byte type; constructor is private
--was export but it didn't make binary work
export
data Byte2 =
  ||| Constructor that boxes two Bit values into a Byte
MkByte2 Bit Bit
{-MkBit and MkByte2 are constructors. We define a byte as a tuple (finite ordered
list of elements) of two bits tagged by the MkBte constructor. We will refer
to the right bit as "bit zero"and the left bit as "bit one." We also call
them the "low order" (b0, right) and "high-order" bit (b1, left).-}


||| Provide a function that packs two bits into and returns a byte
export
byte2_new: Bit -> Bit -> Byte2
byte2_new b1 b0 = MkByte2 b1 b0


||| Projection function returning low-order bit of a Byte2
export
byte2_b0: Byte2 -> Bit
byte2_b0 (MkByte2 b1 b0) = b0


||| Projection function returning high-order bit of a Byte2
export
byte2_b1: Byte2 -> Bit
byte2_b1 (MkByte2 b1 b0) = b1

||| Function that pertwo 2-byte addition
export
byte2_plus: Byte2 -> Byte2 -> Byte2
byte2_plus (MkByte2 b11 b10) (MkByte2 b01 b00) =
  (MkByte2
    (bit_plus3 b11 b10 (bit_carry b00 b01))
    (bit_plus b00 b01))

{-
  b11 b10
+ b01 b00
----------
-}

export
byte2_eq: Byte2 -> Byte2 -> Bool
byte2_eq (MkByte2 b11 b10) (MkByte2 b01 b00) =
  bool_and (bit_eq b10 b00) (bit_eq b11 b01)


||| Implementation of Eq interface for Byte2
export
implementation Eq Byte2 where
  eq b1 b2 = byte2_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
