module Ifthenelse

import public bool

||| If-then-else returning a value of the specified type
export
ifthenelse: {t: Type} -> Bool -> t -> t -> t
ifthenelse True tbranch _ = tbranch
ifthenelse False _ fbranch = fbranch
{-polymorphic: takes a type as a number-}
