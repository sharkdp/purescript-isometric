module Data.List.ThreeOrMore
  ( ThreeOrMore
  , toList
  , reverse
  , last
  ) where

import Prelude ((<>))

import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))

type ThreeOrMore a = NonEmpty (NonEmpty (NonEmpty List)) a

last :: forall a. ThreeOrMore a -> a
last (_ :| _ :| x :| xs) = fromMaybe x (List.last xs)

toList :: forall a. ThreeOrMore a -> List a
toList (a :| b :| c :| xs) = a : b : c : xs

reverse :: forall a. ThreeOrMore a -> ThreeOrMore a
reverse (a :| b :| c :| xs) =
  case List.reverse xs of
    Nil             -> c :| b :| a :| Nil
    d : Nil         -> d :| c :| b :| a : Nil
    e : d : Nil     -> e :| d :| c :| b : a : Nil
    f : e : d : mid -> f :| e :| d :| (mid <> (c : b : a : Nil))
