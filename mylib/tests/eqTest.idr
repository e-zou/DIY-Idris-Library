module eqTest

import eq
import binary
import bit
import bool
import list
import nat
import unit

b1: Bool
b1 = eq B0 B1
--expect False

b2: Bool
b2 = eq B0 B0
--expect True

b3: Bool
b4 = list_eq (Cons B1 (Cons B0 (Cons B1 Nil)) (Cons B1 (Cons B0 (Cons B1 Nil))
-- expect True

b3: Bool
b5 = eq (Cons nat_one (Cons nat_two Nil)) (Cons nat_one (Cons nat_two Nil))
--expect True

a1: Bool
a1 = allEven (Cons nat_one (Cons nat_two (Cons nat_three (Cons nat_four Nil))))
-- expect False

a2: Bool
a2 = allEven (Cons nat_two (Cons nat_four (Cons nat_six Nil)))
-- expect True

a3: Bool
a3 = someEven (Cons nat_one (Cons nat_two (Cons nat_three Nil)))
-- expect True

a4: Bool
a4 = someEven (Cons nat_three (Cons nat_five (Cons nat_nine Nil)))
-- expect False

a5: Nat
a5 = numEven (Cons nat_two (Cons nat_three (Cons nat_four (Cons nat_five (Cons nat_six (Cons nat_seven Nil))))))
-- expect 3

a6: Nat
a6 = list_length' (Cons nat_one (Cons nat_two (Cons nat_three (Cons nat_four (Cons nat_five (Cons nat_six Nil))))))
-- expect 6
