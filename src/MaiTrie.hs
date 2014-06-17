{-# LANGUAGE UnicodeSyntax #-}

module MaiTrie (Trie(..), new, addToTrie) where

import Data.Bifunctor
import Data.Biapplicative

import Data.Foldable
import Data.Bifoldable

import Data.Monoid

import Data.Functor ((<$>))
import Control.Applicative ((<*>))

-- Unicode imports below. (purdy!)
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Eq.Unicode
import Data.Function.Unicode

data Trie k v = End [k] v
              | Node k [Trie k v]
              deriving Show

-- Bifunctors ??
instance Bifunctor Trie where
  -- bimap ∷ (a → b) → (c → d) → Trie a b → Trie c d
  bimap f g (End a b) = End (f <$> a) (g b)
  bimap f g (Node a b) = Node (f a) (bimap f g <$> b)
  -- First ∷ (a → b) → Trie a v → Trie b v
  first f (End a b) = End (f <$> a) b
  first f (Node a b) = Node (f a) (first f <$> b)
  -- second ∷ (b → c) → Trie a b → Trie a c
  second f (End a b) = End a (f b)
  second f (Node a b) = Node a (second f <$> b)

-- Biapplicatives?? WHAT IS THIS MADNESS?
instance Biapplicative Trie where
  -- bipure ∷ a → b → Trie a b
  bipure k v = End [k] v
  -- <<*>> ∷ Trie (a → b) (c → d) → Trie a c → Trie b d
  (<<*>>) (End f g) (End a b) = End (f <*> a) (g b)
  (<<*>>) (End (f:fs) g) (Node a b) = Node (f a) $ fmap ((End fs g) <<*>>) b
  -- Node f@(a → b) g@[Trie (a → b) (c → d)] <<*>> End [a] c
  (<<*>>) (Node f g) (End (a:as) c) = Node (f a) $ fmap (<<*>> (End as c)) g
  (<<*>>) (Node f g) (Node a c) = Node (f a) $ (<<*>>) <$> g <*> c

new ∷ Eq a ⇒ [a] → v → [Trie a v]
new s v = [End s v]

addToTrie ∷ Eq a ⇒ [a] → v → [Trie a v] → [Trie a v]
-- Trying to add an empty list.
addToTrie [] _ t = t

-- No branches match the new input so add a new trie.
addToTrie k v [] = new k v

-- Adding to the end of a branch
addToTrie (a:as) v ((End (b:bs) v'):ts)
  | a ≡ b = [Node a $ addToTrie as v (new bs v')] ++ ts
  | otherwise = End (b:bs) v': addToTrie (a:as) v ts
                
-- Check if we need to make a new branch from this location.
addToTrie (a:as) v ((Node b t'):ts)
  | a ≡ b = [Node a $ addToTrie as v t'] ++ ts
  | otherwise = Node b t': addToTrie (a:as) v ts
