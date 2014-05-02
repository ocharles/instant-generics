{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE EmptyDataDecls           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.Base
-- Copyright   :  (c) 2010, Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module defines the basic representation types and the conversion
-- functions 'to' and 'from'. A typical instance for a user-defined datatype
-- would be:
--
-- > -- Example datatype
-- > data Exp = Const Int | Plus Exp Exp
-- >
-- > -- Auxiliary datatypes for constructor representations
-- > data Const
-- > data Plus
-- >
-- > instance Constructor Const where conName _ = "Const"
-- > instance Constructor Plus  where conName _ = "Plus"
-- >
-- > -- Representable instance
-- > instance Representable Exp where
-- >   type Rep Exp = C Const (Var Int) :+: C Plus (Rec Exp :*: Rec Exp)
-- >
-- >   from (Const n)   = L (C (Var n))
-- >   from (Plus e e') = R (C (Rec e :*: Rec e'))
-- >
-- >   to (L (C (Var n)))            = Const n
-- >   to (R (C (Rec e :*: Rec e'))) = Plus e e'
--
-----------------------------------------------------------------------------

module Generics.Instant.Base (
      Z, U(..), (:+:)(..), (:*:)(..), CEq(..), C, Var(..), Rec(..)
    , Constructor(..), Fixity(..), Associativity(..)
    , Representable(..)
    , X, Ze, Su
  ) where

import GHC.Exts (Constraint)

infixr 5 :+:
infixr 6 :*:

data Z
data U       = U              deriving (Show, Read)
data a :+: b = L a | R b      deriving (Show, Read)
data a :*: b = a :*: b        deriving (Show, Read)
data Var a   = Var a          deriving (Show, Read)
data Rec a   = Rec a          deriving (Show, Read)

data CEq k c p a where C :: k => a -> CEq k c p a
deriving instance (Show a) => Show (CEq k c p a)
--deriving instance (Read a) => Read (CEq k c p p a)

-- Shorthand when no proofs are required
type C c a = CEq () c () a

-- | Class for datatypes that represent data constructors.
-- For non-symbolic constructors, only 'conName' has to be defined.
class Constructor c where
  conName   :: t (k :: Constraint) c p a -> String
  {-# INLINE conFixity #-}
  conFixity :: t (k :: Constraint) c p a -> Fixity
  conFixity = const Prefix
  {-# INLINE conIsRecord #-}
  conIsRecord :: t (k :: Constraint) c p a -> Bool
  conIsRecord = const False

-- | Datatype to represent the fixity of a constructor. An infix declaration
-- directly corresponds to an application of 'Infix'.
data Fixity = Prefix | Infix Associativity Int
  deriving (Eq, Show, Ord, Read)

-- | Datatype to represent the associativy of a constructor.
data Associativity = LeftAssociative | RightAssociative | NotAssociative
  deriving (Eq, Show, Ord, Read)


class Representable a where
  type Rep a
  to   :: Rep a -> a
  from :: a -> Rep a
  -- defaults
  {-
  type Rep a = a -- type synonyms defaults are not yet implemented!
  to   = id
  from = id
  -}

-- Type family for representing existentially-quantified variables
type family X c n a

-- Type-level natural numbers
data Ze :: *
data Su :: * -> *
