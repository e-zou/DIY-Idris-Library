module uint8

import public bit
import public nat
import ifthenelse
import byte8
import eq

||| Private constructor that boxes a 8-bit Byte into unassigned byte8
export
data UInt8 =
  MkUInt8 Byte8

||| Packs an 8-bit byte into an unassigned byte8
export
uint_new: Byte8 -> UInt8
uint_new aByte8 = MkUInt8 aByte8

||| Unpacks an unassigned byte8 into a regular byte8
export
get_byte8: UInt8 -> Byte8
get_byte8 (MkUInt8 aByte8) = aByte8

||| UInt value that represents 0
export
uint8_zero: UInt8
uint8_zero = uint_new byte8_zeros

||| Constant UInt8 value that equals one
export
uint8_one: UInt8
uint8_one = uint_new byte8one

||| Constant UInt8 value that equals two
export
uint8_two: UInt8
uint8_two = uint_new byte8two

||| Constant UInt8 value that equals three
export
uint8_three: UInt8
uint8_three = uint_new byte8three

||| Constant UInt8 value that equals four
export
uint8_four: UInt8
uint8_four = uint_new byte8four

||| Constant UInt8 value that equals five
export
uint8_five: UInt8
uint8_five = uint_new byte8five

||| Adds two unassigned 8-bit bytes together
export
uint8_plus: UInt8 -> UInt8 -> UInt8
uint8_plus aUInt8one aUInt8two =
  MkUInt8 (byte8_plus (get_byte8 aUInt8one) (get_byte8 aUInt8two))

||| Adds 255 to an unassigned 8-bit byte
export
uint8_dec: UInt8 -> UInt8
uint8_dec aUInt8 =
  MkUInt8 (byte8_plus (get_byte8 aUInt8) byte8_ones)

||| Increment a UInit8 value by one
export
uint8_inc: UInt8 -> UInt8
uint8_inc aUInt8 = uint8_plus aUInt8 uint8_one

||| Boolean expression that evaluates True iff two UInt8 values are equal
export
uint8_eq: UInt8 -> UInt8 -> Bool
uint8_eq (MkUInt8 aByte8) (MkUInt8 otherByte8) =
  byte8_eq aByte8 otherByte8

||| return True iff given UInt value represents 0
export
true_if_zero: UInt8 -> Bool
true_if_zero aUInt8 = uint8_eq aUInt8 uint8_zero

||| Implementation of Eq interface for Byte8
export
implementation Eq UInt8 where
  eq b1 b2 = uint8_eq b1 b2
  neq b1 b2 = bool_not (eq b1 b2)
