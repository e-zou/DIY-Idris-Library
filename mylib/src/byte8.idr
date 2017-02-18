||| Abstract data type representing two-bit byte
module byte8

import public bit
import public nat
import ifthenelse
import eq

||| A two-bit byte type; constructor is private
export
data Byte8 =
  ||| Constructor that boxes eight Bit values into a Byte8 value
  MkByte8 Bit Bit Bit Bit Bit Bit Bit Bit

||| Pack eight bits into, and return, a byte (a 8-tuple of bits)
export
byte8_new: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Byte8
byte8_new b7 b6 b5 b4 b3 b2 b1 b0 = MkByte8 b7 b6 b5 b4 b3 b2 b1 b0

-- Constant byte8 values filled with zeros and ones, resp.

export
byte8_zeros: Byte8
byte8_zeros = byte8_new B0 B0 B0 B0 B0 B0 B0 B0

export
byte8_ones: Byte8
byte8_ones = byte8_new B1 B1 B1 B1 B1 B1 B1 B1

export
byte8one: Byte8
byte8one = byte8_new B0 B0 B0 B0 B0 B0 B0 B1

export
byte8two: Byte8
byte8two = byte8_new B0 B0 B0 B0 B0 B0 B1 B0

export
byte8three: Byte8
byte8three = byte8_new B0 B0 B0 B0 B0 B0 B1 B1

export
byte8four: Byte8
byte8four = byte8_new B0 B0 B0 B0 B0 B1 B0 B0

export
byte8five: Byte8
byte8five = byte8_new B0 B0 B0 B0 B0 B1 B0 B1

-- Projection functions, returning particular component bits

||| Return Bit 0
export
byte8_bit0: Byte8 -> Bit
byte8_bit0 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b0

||| Return Bit 1
export
byte8_bit1: Byte8 -> Bit
byte8_bit1 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b1

||| Return Bit 2
export
byte8_bit2: Byte8 -> Bit
byte8_bit2 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b2

||| Return Bit 3
export
byte8_bit3: Byte8 -> Bit
byte8_bit3 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b3


||| Return Bit 3
export
byte8_bit4: Byte8 -> Bit
byte8_bit4 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b4


||| Return Bit 3
export
byte8_bit5: Byte8 -> Bit
byte8_bit5 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b5


||| Return Bit 3
export
byte8_bit6: Byte8 -> Bit
byte8_bit6 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b6


||| Return Bit 3
export
byte8_bit7: Byte8 -> Bit
byte8_bit7 (MkByte8 b7 b6 b5 b4 b3 b2 b1 b0) = b7


-- arithmetic functions on 8-bit bytes

||| Return the indexed bit of the given byte or B0 if the index is out of range
export
byte8_sub: Nat -> Byte8 -> Bit
byte8_sub n aByte8 =
  ifthenelse
    (nat_eq n nat_zero)
    (byte8_bit0 aByte8)
    (ifthenelse
      (nat_eq n nat_one)
      (byte8_bit1 aByte8)
      (ifthenelse
        (nat_eq n nat_two)
        (byte8_bit2 aByte8)
        (ifthenelse
          (nat_eq n nat_three)
          (byte8_bit3 aByte8)
          (ifthenelse
            (nat_eq n nat_four)
            (byte8_bit4 aByte8)
            (ifthenelse
              (nat_eq n nat_five)
              (byte8_bit5 aByte8)
              (ifthenelse
                (nat_eq n nat_six)
                (byte8_bit6 aByte8)
                (ifthenelse
                  (nat_eq n nat_seven)
                  (byte8_bit7 aByte8)
                  (B0))))))))

||| Return the carry bit in the indexed column when adding the two bytes
||| or the zero bit (B0) if the index is out of range
export
carry_sub8: Nat -> Byte8 -> Byte8 -> Bit
carry_sub8 n b1 b0 =
            ifthenelse
              (nat_eq n nat_zero)
              (B0)
              (bit_carry3
                (carry_sub8 (nat_pred n) b1 b0)
                (byte8_sub (nat_pred n) b1)
                (byte8_sub (nat_pred n) b0))


||| Return the indexed bit of the sum of the given bytes
export
sum_sub8: Nat -> Byte8 -> Byte8 -> Bit
sum_sub8 n b1 b0 =
          (bit_plus3
            (carry_sub8 n b1 b0)
            (byte8_sub n b1)
            (byte8_sub n b0))

||| Return the sum of two 8-bit bytes as a 8-bit byte ignoring overflows
export
byte8_plus: Byte8 -> Byte8 -> Byte8
byte8_plus b0 b1 =
  byte8_new
    (sum_sub8 nat_seven b1 b0)
    (sum_sub8 nat_six b1 b0)
    (sum_sub8 nat_five b1 b0)
    (sum_sub8 nat_four b1 b0)
    (sum_sub8 nat_three b1 b0)
    (sum_sub8 nat_two b1 b0)
    (sum_sub8 nat_one b1 b0)
    (sum_sub8 nat_zero b1 b0)



-- a little test case
n: Byte8
n = byte8_plus byte8_ones byte8_ones
-- expect 11111110


export
byte8_eq: Byte8 -> Byte8 -> Bool
byte8_eq (MkByte8 b17 b16 b15 b14 b13 b12 b11 b10) (MkByte8 b07 b06 b05 b04 b03 b02 b01 b00) =
  bool_and
    (bool_and
      (bool_and (bit_eq b10 b00) (bit_eq b11 b01))
      (bool_and (bit_eq b12 b02) (bit_eq b13 b03)))
    (bool_and
      (bool_and (bit_eq b14 b04) (bit_eq b15 b05))
      (bool_and (bit_eq b16 b06) (bit_eq b17 b07)))


export
byte8_and: Byte8 -> Byte8 -> Byte8
byte8_and b1 b0 =
  byte8_new
    (bit_and (byte8_bit7 b1) (byte8_bit7 b0))
    (bit_and (byte8_bit6 b1) (byte8_bit6 b0))
    (bit_and (byte8_bit5 b1) (byte8_bit5 b0))
    (bit_and (byte8_bit4 b1) (byte8_bit4 b0))
    (bit_and (byte8_bit3 b1) (byte8_bit3 b0))
    (bit_and (byte8_bit2 b1) (byte8_bit2 b0))
    (bit_and (byte8_bit1 b1) (byte8_bit1 b0))
    (bit_and (byte8_bit0 b1) (byte8_bit0 b0))


export
byte8_or: Byte8 -> Byte8 -> Byte8
byte8_or b1 b0  =
  byte8_new
    (bit_or (byte8_bit7 b1) (byte8_bit7 b0))
    (bit_or (byte8_bit6 b1) (byte8_bit6 b0))
    (bit_or (byte8_bit5 b1) (byte8_bit5 b0))
    (bit_or (byte8_bit4 b1) (byte8_bit4 b0))
    (bit_or (byte8_bit3 b1) (byte8_bit3 b0))
    (bit_or (byte8_bit2 b1) (byte8_bit2 b0))
    (bit_or (byte8_bit1 b1) (byte8_bit1 b0))
    (bit_or (byte8_bit0 b1) (byte8_bit0 b0))

export
byte8_not: Byte8 -> Byte8
byte8_not b0 =
  byte8_new
    (bit_not (byte8_bit7 b0))
    (bit_not (byte8_bit6 b0))
    (bit_not (byte8_bit5 b0))
    (bit_not (byte8_bit4 b0))
    (bit_not (byte8_bit3 b0))
    (bit_not (byte8_bit2 b0))
    (bit_not (byte8_bit1 b0))
    (bit_not (byte8_bit0 b0))

||| Implementation of Eq interface for Byte8
export
implementation Eq Byte8 where
  eq b1 b2 = byte8_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
