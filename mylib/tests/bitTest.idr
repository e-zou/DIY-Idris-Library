module bitTest

{-Import both bit and bool because we define both data types. b0 is Bit while
r2 is Bool.-}
import bit
import bool

b0: Bit
b0 = B0
b1: Bit
b1 = B1

r1: Bool
{-Using Bool because data type in bit.idr is from Bit -> Bool-}
r1 = bit_rep b0
{-Use b0 since in this code, b0 represents B0 with data type Bit, which
the function bit_rep uses-}
r2: Bool
r2 = bit_rep b1

p1: Bit
{-Using Bool because data type in bit.idr is from Bit -> Bit -> Bit-}
p1 = bit_plus b0 b1
-- expect b1
p2: Bit
p2 = bit_plus b1 b0
-- expect b1
p3: Bit
p3 = bit_plus b1 b1
-- expect b0
p4: Bit
p4 = bit_plus b0 b0

c1: Bit
c1 = bit_carry b1 b0
c2: Bit
c2 = bit_carry b0 b1
c3: Bit
c3 = bit_carry b1 b1
c4: Bit
c4 = bit_carry b0 b0
