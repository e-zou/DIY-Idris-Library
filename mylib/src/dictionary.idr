||| Implements a dictionary abstract data type
module dictionary

import list
import pair
import public option
import ifthenelse

||| A dictionary data type with a private implemented
-- hides dictionary are built from lists
export
data Dictionary k v =
  MkDictionary (List (Pair k v))

||| A public function to build and return an empty dictionary
export
dictionary_new: Dictionary k v
dictionary_new = MkDictionary Nil

||| Function to insert (k, v) into dictionary. IF there was no previous
||| value for k in the dictionary, v will now be that value. If there was
||| already a value for k, v will be the new value for k. In other words,
||| dictionary_insert "overwrites" any previous definition. The pair is
||| added to the head of the list, so it returns the key value pair that
||| matches first.
-- l for list, p for pair
export
dictionary_insert: Dictionary k v -> Pair k v -> Dictionary k v
dictionary_insert (MkDictionary l) p =
  MkDictionary (Cons p l)

||| A private helper function in support of dictionary_lookup
dict_list_lookup: Eq k => List (Pair k v) -> k -> Option v
dict_list_lookup Nil k = None
dict_list_lookup (Cons h t) k =
  ifthenelse
  (eq k (pair_key h))
  (Some (pair_value h))
  (dict_list_lookup t k)

||| Function look up and return the value for a given key if there is one.
export
dictionary_lookup: Eq k => (dict: Dictionary k v) -> (key: k) -> Option v
dictionary_lookup (MkDictionary l) k = dict_list_lookup l k
