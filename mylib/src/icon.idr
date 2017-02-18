module icon

import byte8
import bool
import ifthenelse
import nat

export
data Icon = MkIcon Byte8 Byte8 Byte8 Byte8 Byte8 Byte8 Byte8 Byte8

export
gambly: Icon
gambly =
  MkIcon
  (byte8_new (bit_new True) (bit_new True) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new True) (bit_new True))
  (byte8_new (bit_new True) (bit_new False) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new False) (bit_new True))
  (byte8_new (bit_new False) (bit_new True) (bit_new False) (bit_new True) (bit_new False) (bit_new True) (bit_new True) (bit_new False))
  (byte8_new (bit_new False) (bit_new True) (bit_new False) (bit_new True) (bit_new False) (bit_new True) (bit_new True) (bit_new False))
  (byte8_new (bit_new False) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new False) (bit_new True) (bit_new False))
  (byte8_new (bit_new False) (bit_new True) (bit_new False) (bit_new False) (bit_new False) (bit_new True) (bit_new True) (bit_new False))
  (byte8_new (bit_new True) (bit_new False) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new False) (bit_new True))
  (byte8_new (bit_new True) (bit_new True) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new True) (bit_new True))



export
row: Nat -> Icon -> Byte8
row n (MkIcon b0 b1 b2 b3 b4 b5 b6 b7) =
  ifthenelse
    (nat_eq n nat_zero)
    (b0)
    (ifthenelse
      (nat_eq n nat_one)
      (b1)
      (ifthenelse
        (nat_eq n nat_two)
        (b2)
        (ifthenelse
          (nat_eq n nat_three)
          (b3)
          (ifthenelse
            (nat_eq n nat_four)
            (b4)
            (ifthenelse
              (nat_eq n nat_five)
              (b5)
              (ifthenelse
                (nat_eq n nat_six)
                (b6)
                (ifthenelse
                  (nat_eq n nat_seven)
                  (b7)
                  (byte8_zeros))))))))

export
mask: Icon -> Icon -> Icon
mask image mask =
  MkIcon
    (byte8_and (row nat_zero image) (row nat_zero mask))
    (byte8_and (row nat_one image) (row nat_one mask))
    (byte8_and (row nat_two image) (row nat_two mask))
    (byte8_and (row nat_three image) (row nat_three mask))
    (byte8_and (row nat_four image) (row nat_four mask))
    (byte8_and (row nat_five image) (row nat_five mask))
    (byte8_and (row nat_six image) (row nat_six mask))
    (byte8_and (row nat_seven image) (row nat_seven mask))

export
topHalf: Icon
topHalf =
  MkIcon
    (byte8_new (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True))
    (byte8_new (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True))
    (byte8_new (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True))
    (byte8_new (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True) (bit_new True))
    (byte8_new (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False))
    (byte8_new (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False))
    (byte8_new (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False))
    (byte8_new (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False) (bit_new False))

export
maskedResult: Icon
maskedResult = mask gambly topHalf

export
doubleExposure: Icon -> Icon -> Icon
doubleExposure image mask =
  MkIcon
    (byte8_or (row nat_zero image) (row nat_zero mask))
    (byte8_or (row nat_one image) (row nat_one mask))
    (byte8_or (row nat_two image) (row nat_two mask))
    (byte8_or (row nat_three image) (row nat_three mask))
    (byte8_or (row nat_four image) (row nat_four mask))
    (byte8_or (row nat_five image) (row nat_five mask))
    (byte8_or (row nat_six image) (row nat_six mask))
    (byte8_or (row nat_seven image) (row nat_seven mask))

export
doubleExposureResult: Icon
doubleExposureResult = doubleExposure gambly topHalf
