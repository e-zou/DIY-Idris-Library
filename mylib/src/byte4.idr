||| 4-bit byte abstract data type
module byte4

import public bit
import public nat
import ifthenelse
import eq

||| The constructor MkByte makes four bits into a byte
export
data Byte4 =
  ||| Constructor that boxes four Bit values into a Byte
MkByte4 Bit Bit Bit Bit


||| Byte4 is made out of four bits
export
byte4_new: Bit -> Bit -> Bit -> Bit -> Byte4
byte4_new b3 b2 b1 b0 = MkByte4 b3 b2 b1 b0

-- Constant byte4 vales filled with zeros and ones

export
byte4_zeros: Byte4
byte4_zeros = byte4_new B0 B0 B0 B0

export
byte4_ones: Byte4
byte4_ones = byte4_new B1 B1 B1 B1

-- Projection functions, returning particular component bits

||| function returning the rightmost bit
export
byte4_bit0: Byte4 -> Bit
byte4_bit0 (MkByte4 b3 b2 b1 b0) = b0

||| function returning the 2nd rightmost bit
export
byte4_bit1: Byte4 -> Bit
byte4_bit1 (MkByte4 b3 b2 b1 b0) = b1

||| function returning the 3rd rightmost bit
export
byte4_bit2: Byte4 -> Bit
byte4_bit2 (MkByte4 b3 b2 b1 b0) = b2

||| function returning the leftmost bit
export
byte4_bit3: Byte4 -> Bit
byte4_bit3 (MkByte4 b3 b2 b1 b0) = b3


||| Representing the addition of two 4-bit bytes
export
byte4_plus: Byte4 -> Byte4 -> Byte4
byte4_plus (MkByte4 b13 b12 b11 b10) (MkByte4 b03 b02 b01 b00) =
  (MkByte4
    (bit_plus3 b13 b03 (bit_carry3 b12 b02 (bit_carry3 b11 b01 (bit_carry b10 b00))))
    (bit_plus3 b12 b02 (bit_carry3 b11 b01 (bit_carry b10 b00)))
    (bit_plus3 b11 b01 (bit_carry b10 b00))
    (bit_plus b10 b00)
    )
-- Computes the indexed bit of the sum of the given bytes

-- Can't use the constructors due to abstracting nat.idr through simply exporting only.
{-||| Return the indexed bit of the given byte or B0 if the index is out of range
export
byte4_sub: Nat -> Byte4 -> Bit
byte4_sub n b =
          byte4_sub Z b = byte4_bit0 b
          byte4_sub (S Z) b = byte4_bit1 b
          byte4_sub (S(S Z)) b = byte4_bit2 b
          byte4_sub (S(S(S Z))) b = byte4_bit3 b
          byte4_sub _ b = B0-}

{-ite: (T:Type) -> B -> T -> T -> T
  ite t True tb fb = tb
                    fb-}
||| Returns the indexed bit of the given byte or B0 if the index is out of range
export
byte4_sub: Nat -> Byte4 -> Bit
byte4_sub n aByte4 =
  ifthenelse
  (nat_eq n nat_zero)
  (byte4_bit0 aByte4)
  (ifthenelse
     (nat_eq n nat_one)
     (byte4_bit1 aByte4)
     (ifthenelse
         (nat_eq n nat_two)
         (byte4_bit2 aByte4)
         (ifthenelse
             (nat_eq n nat_three)
             (byte4_bit3 aByte4)
             (B0))))

||| Return the carry bit in the indexed column when adding the two bytes
||| or the zero bit (B0) if the index is out of range
export
carry_sub: Nat -> Byte4 -> Byte4 -> Bit
carry_sub n b1 b0 =
          ifthenelse
            (nat_eq n nat_zero)
            (B0)
            (bit_carry3
              (carry_sub (nat_pred n) b1 b0)
              (byte4_sub (nat_pred n) b1)
              (byte4_sub (nat_pred n) b0)
            )


||| Add three bits in one column of byte addition problem
||| Adds index number plus the number in row one and row two
export
sum_sub: Nat -> Byte4 -> Byte4 -> Bit
sum_sub n b1 b0 =
        (bit_plus3
          (carry_sub n b1 b0)
          (byte4_sub n b1)
          (byte4_sub n b0)
        )

||| Addition
export
byte4_plus': Byte4 -> Byte4 -> Byte4
byte4_plus' b0 b1 =
  (byte4_new
    (sum_sub nat_three b1 b0)
    (sum_sub nat_two b1 b0)
    (sum_sub nat_one b1 b0)
    (sum_sub nat_zero b1 b0)
    )

n: Byte4
n = byte4_plus' byte4_ones byte4_ones

||| If the bytes are the same, then true, otherwise, false
export
byte4_eq: Byte4 -> Byte4 -> Bool
byte4_eq (MkByte4 b13 b12 b11 b10) (MkByte4 b03 b02 b01 b00) =
  bool_and
    (bool_and
      (bit_eq b13 b03)
      (bit_eq b12 b02))
    (bool_and
      (bit_eq b11 b01)
      (bit_eq b10 b00))
{-
ifthenelse
(bit_eq b13 b03)
  ifthenelse
  (bit_eq b12 b02)
    ifthenelse
    (bit_eq b11 b01)
      ifthenelse
      (bit_eq b10 b00)
-}


||| Implementation of Eq interface for Byte4
export
implementation Eq Byte4 where
  eq b1 b2 = byte4_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
