||| A library of funcitons for binary arithmetic
module binary

import byte2
import eq

||| Implementation of half-adder
export
half_adder: Bit -> Bit -> Byte2
half_adder b1 b0 =
  byte2_new
    (bit_carry b1 b0)
    (bit_plus b1 b0)

||| Implement a full adder
export
full_adder: Bit -> Bit -> Bit -> Byte2
full_adder b1 b2 cin =
  byte2_new
    (bit_carry3 b1 b2 cin)
    (bit_carry3 b1 b2 cin)
