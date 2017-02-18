module binaryTest

import bit
import bool
import byte2
import binary

-- some cdtest code, should be refactored into test file
sum0: Byte2
sum0 = full_adder B0 B0 B0
-- expect MkByte2 B0 B0

sum1: Byte2
sum1 = full_adder B0 B0 B1
-- expect MkByte2 B1 B0
{-first variable, B1 is sum; second variable, B0 is carry-}

sum2: Byte2
sum2 = full_adder B0 B1 B0
-- expect MkByte2 B1 B0

sum3: Byte2
sum3 = full_adder B0 B1 B1
-- expect MkByte2 B0 B1

sum4: Byte2
sum4 = full_adder B1 B0 B0
-- expect MkByte2 B1 B0

sum5: Byte2
sum5 = full_adder B1 B1 B0
-- expect MkByte2 B0 B1

sum6: Byte2
sum6 = full_adder B1 B0 B1
-- expect MkByte2 B0 B1

sum7: Byte2
sum7 = full_adder B1 B1 B1
-- expect MkByte B1 B1
