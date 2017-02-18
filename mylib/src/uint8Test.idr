module uint8Test

import uint8
import byte8
import bit

a1: UInt8
a1 = uint_new (byte8_zeros)
-- Expect MkByte8 B0 B0 B0 B0 B0 B0 B0 B0

a2: Byte8
a2 = get_byte8 (uint8_zero)
-- Expect MkByte8 B0 B0 B0 B0 B0 B0 B0 B0

a3: UInt8
a3 = uint8_plus (uint8_zero) (uint8_one)
-- Expect MkUInt8 (MkByte8 B0 B0 B0 B0 B0 B0 B0 B1)

a4: UInt8
a4 = uint8_inc (uint8_one)
-- Expect MkUInt8 MkByte8 B0 B0 B0 B0 B0 B0 B1 B0

a5: Bool
a5 = uint8_eq uint8_zero uint8_one
-- Expect False

a6: Bool
a6 = true_if_zero uint8_zero
-- Expect True

a7: UInt8
a7 = uint8_dec uint8_one
-- Expect MkUInt8 MkByte8 B0 B0 B0 B0 B0 B0 B1 B0

a8: UInt8
a8 = uint8_dec uint8_two
