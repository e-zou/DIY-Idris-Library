||| A module implementing a polymorphic pair abstract and data type

module Pair

import nat
import bool

||| A polymorphic pair type
public export
data Pair k v =
  MkPair k v

export
pair_first: Pair k v -> k
pair_first (MkPair k v) = k

export
pair_second: Pair k v -> v
pair_second (MkPair k v) = v

export
pair_key: Pair k v -> k
pair_key p = pair_first p

export
pair_value: Pair k v -> v
pair_value p = pair_second p
