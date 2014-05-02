{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Terms where

import Generics.Instant
import Generics.Instant.Functions.Eq
import Generics.Instant.Functions.Show
import Generics.Instant.TH

data Term a where
  Eq :: Term Int -> Term Int -> Term Bool

eqTerms :: Term a -> String
eqTerms = gshowDefault

deriveAll ''Term

--type RepTerm_ a = CEq (a ~ Bool) () a (Rec (Term Int))

{-

type RepTerm_ a_aiTB =
        CEq (a_aiTB (GHC.Types.~) Bool, ())
            Term_Eq_
            ((:*:) a_aiTB ())
            ((:*:) (Rec (Term Int))
            (Rec (Term Int)))

-}
