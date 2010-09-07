{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -w           #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Instant.TH
-- Copyright   :  (c) 2010 Universiteit Utrecht
-- License     :  BSD3
--
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module contains Template Haskell code that can be used to
-- automatically generate the boilerplate code for the generic deriving
-- library.
-----------------------------------------------------------------------------

-- Adapted from Generics.Deriving.TH
module Generics.Instant.TH (
      deriveAll
    , deriveConstructors
    , deriveRepresentable
    , deriveRep
  ) where

import Generics.Instant.Base

import Language.Haskell.TH hiding (Fixity())
import Language.Haskell.TH.Syntax (Lift(..))

import Data.List (intercalate)
import Control.Monad


-- | Given the type and the name (as string) for the type to derive,
-- generate the 'Constructor' instances and the 'Representable' instance.
deriveAll :: Name -> Q [Dec]
deriveAll n =
  do a <- deriveConstructors n
     b <- deriveRepresentable n
     return (a ++ b)

-- | Given a datatype name, derive datatypes and 
-- instances of class 'Constructor'.
deriveConstructors :: Name -> Q [Dec]
deriveConstructors = constrInstance

-- | Given the type and the name (as string) for the Representable type
-- synonym to derive, generate the 'Representable' instance.
deriveRepresentable :: Name -> Q [Dec]
deriveRepresentable n = do
    rep <- deriveRep n
    inst <- deriveInst n
    return $ rep ++ inst

-- | Derive only the 'Rep' type synonym. Not needed if 'deriveRepresentable'
-- is used.
deriveRep :: Name -> Q [Dec]
deriveRep n = do
  i <- reify n
  fmap (:[]) $ tySynD (genRepName n) (typeVariables i) (repType n)

deriveInst :: Name -> Q [Dec]
deriveInst t = do
  i <- reify t
  let typ q = return $ foldl (\a -> AppT a . VarT . tyVarBndrToName) (ConT q) 
                (typeVariables i)
  fcs <- mkFrom t 1 0 t
  tcs <- mkTo   t 1 0 t
  liftM (:[]) $
    instanceD (cxt [])
      (conT ''Representable `appT` typ t)
        [ tySynInstD ''Rep [typ t] (typ (genRepName t))
        , funD 'from fcs, funD 'to tcs]

constrInstance :: Name -> Q [Dec]
constrInstance n = do
  i <- reify n
  case i of
    TyConI (DataD    _ n _ cs _) -> mkInstance n cs
    TyConI (NewtypeD _ n _ c  _) -> mkInstance n [c]
    _ -> return []
  where
    mkInstance n cs = do
      ds <- mapM (mkConstrData n) cs
      is <- mapM (mkConstrInstance n) cs
      return $ ds ++ is

typeVariables :: Info -> [TyVarBndr]
typeVariables (TyConI (DataD    _ _ tv _ _)) = tv
typeVariables (TyConI (NewtypeD _ _ tv _ _)) = tv
typeVariables _                           = []

tyVarBndrToName :: TyVarBndr -> Name
tyVarBndrToName (PlainTV  name)   = name
tyVarBndrToName (KindedTV name _) = name

stripRecordNames :: Con -> Con
stripRecordNames (RecC n f) =
  NormalC n (map (\(_, s, t) -> (s, t)) f)
stripRecordNames c = c

genName :: [Name] -> Name
genName = mkName . (++"_") . intercalate "_" . map nameBase

genRepName :: Name -> Name
genRepName = mkName . (++"_") . ("Rep"  ++) . nameBase

mkConstrData :: Name -> Con -> Q Dec
mkConstrData dt (NormalC n _) =
  dataD (cxt []) (genName [dt, n]) [] [] [] 
mkConstrData dt r@(RecC _ _) =
  mkConstrData dt (stripRecordNames r)
mkConstrData dt (InfixC t1 n t2) =
  mkConstrData dt (NormalC n [t1,t2])

instance Lift Fixity where
  lift Prefix      = conE 'Prefix
  lift (Infix a n) = conE 'Infix `appE` [| a |] `appE` [| n |]

instance Lift Associativity where
  lift LeftAssociative  = conE 'LeftAssociative
  lift RightAssociative = conE 'RightAssociative
  lift NotAssociative   = conE 'NotAssociative

mkConstrInstance :: Name -> Con -> Q Dec
mkConstrInstance dt (NormalC n _) = mkConstrInstanceWith dt n []
mkConstrInstance dt (RecC    n _) = mkConstrInstanceWith dt n
      [ funD 'conIsRecord [clause [wildP] (normalB (conE 'True)) []]]
mkConstrInstance dt (InfixC t1 n t2) =
    do
      i <- reify n
      let fi = case i of
                 DataConI _ _ _ f -> convertFixity f
                 _ -> Prefix
      instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
        [funD 'conName   [clause [wildP] (normalB (stringE (nameBase n))) []],
         funD 'conFixity [clause [wildP] (normalB [| fi |]) []]]
  where
    convertFixity (Fixity n d) = Infix (convertDirection d) n
    convertDirection InfixL = LeftAssociative
    convertDirection InfixR = RightAssociative
    convertDirection InfixN = NotAssociative

mkConstrInstanceWith :: Name -> Name -> [Q Dec] -> Q Dec
mkConstrInstanceWith dt n extra = 
  instanceD (cxt []) (appT (conT ''Constructor) (conT $ genName [dt, n]))
    (funD 'conName [clause [wildP] (normalB (stringE (nameBase n))) []] : extra)

repType :: Name -> Q Type
repType n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  (foldr1' sum (error "Empty datatypes are not supported.")
                    (map (repCon (dt, map tyVarBndrToName vs)) cs))
                TyConI (NewtypeD _ dt vs c _) ->
                  repCon (dt, map tyVarBndrToName vs) c
                TyConI (TySynD t _ _) -> error "type synonym?" 
                _ -> error "unknown construct" 
      --appT b (conT $ mkName (nameBase n))
      b where
    sum :: Q Type -> Q Type -> Q Type
    sum a b = conT ''(:+:) `appT` a `appT` b


repCon :: (Name, [Name]) -> Con -> Q Type
repCon (dt, vs) (NormalC n []) =
    conT ''C `appT` (conT $ genName [dt, n]) `appT` conT ''U
repCon (dt, vs) (NormalC n fs) =
    conT ''C `appT` (conT $ genName [dt, n]) `appT` 
     (foldr1 prod (map (repField (dt, vs) . snd) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b
repCon (dt, vs) r@(RecC n []) =
    conT ''C `appT` (conT $ genName [dt, n]) `appT` conT ''U
repCon (dt, vs) r@(RecC n fs) =
    conT ''C `appT` (conT $ genName [dt, n]) `appT` 
      (foldr1 prod (map (repField' (dt, vs) n) fs)) where
    prod :: Q Type -> Q Type -> Q Type
    prod a b = conT ''(:*:) `appT` a `appT` b

repCon d (InfixC t1 n t2) = repCon d (NormalC n [t1,t2])

--dataDeclToType :: (Name, [Name]) -> Type
--dataDeclToType (dt, vs) = foldl (\a b -> AppT a (VarT b)) (ConT dt) vs

repField :: (Name, [Name]) -> Type -> Q Type
--repField d t | t == dataDeclToType d = conT ''I
repField d t = conT ''Rec `appT` return t

repField' :: (Name, [Name]) -> Name -> (Name, Strict, Type) -> Q Type
--repField' d ns (_, _, t) | t == dataDeclToType d = conT ''I
repField' (dt, vs) ns (f, _, t) = conT ''Rec `appT` return t
-- Note: we should generate Var too, at some point


mkFrom :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkFrom ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapE e = lrE m i e
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  zipWith (fromCon wrapE ns (dt, map tyVarBndrToName vs)
                    (length cs)) [0..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [fromCon wrapE ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD t _ _) -> error "type synonym?" 
                  -- [clause [varP (field 0)] (normalB (wrapE $ conE 'K1 `appE` varE (field 0))) []]
                _ -> error "unknown construct"
      return b

mkTo :: Name -> Int -> Int -> Name -> Q [Q Clause]
mkTo ns m i n =
    do
      -- runIO $ putStrLn $ "processing " ++ show n
      let wrapP p = lrP m i p
      i <- reify n
      let b = case i of
                TyConI (DataD _ dt vs cs _) ->
                  zipWith (toCon wrapP ns (dt, map tyVarBndrToName vs)
                    (length cs)) [0..] cs
                TyConI (NewtypeD _ dt vs c _) ->
                  [toCon wrapP ns (dt, map tyVarBndrToName vs) 1 0 c]
                TyConI (TySynD t _ _) -> error "type synonym?" 
                  -- [clause [wrapP $ conP 'K1 [varP (field 0)]] (normalB $ varE (field 0)) []]
                _ -> error "unknown construct" 
      return b

fromCon :: (Q Exp -> Q Exp) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
fromCon wrap ns (dt, vs) m i (NormalC cn []) =
  clause
    [conP cn []]
    (normalB $ wrap $ lrE m i $ appE (conE 'C) $ conE 'U) []
fromCon wrap ns (dt, vs) m i (NormalC cn fs) =
  -- runIO (putStrLn ("constructor " ++ show ix)) >>
  clause
    [conP cn (map (varP . field) [0..length fs - 1])]
    (normalB $ wrap $ lrE m i $ conE 'C `appE` 
      foldr1 prod (zipWith (fromField (dt, vs)) [0..] (map snd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns (dt, vs) m i r@(RecC cn []) =
  clause
    [conP cn []]
    (normalB $ wrap $ lrE m i $ conE 'C `appE` (conE 'U)) []
fromCon wrap ns (dt, vs) m i r@(RecC cn fs) =
  clause
    [conP cn (map (varP . field) [0..length fs - 1])]
    (normalB $ wrap $ lrE m i $ conE 'C `appE` 
      foldr1 prod (zipWith (fromField (dt, vs)) [0..] (map trd fs))) []
  where prod x y = conE '(:*:) `appE` x `appE` y
fromCon wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  fromCon wrap ns (dt, vs) m i (NormalC cn [t1,t2])

fromField :: (Name, [Name]) -> Int -> Type -> Q Exp
--fromField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conE 'I `appE` varE (field nr)
fromField (dt, vs) nr t = conE 'Rec `appE` varE (field nr)

toCon :: (Q Pat -> Q Pat) -> Name -> (Name, [Name]) -> Int -> Int -> Con -> Q Clause
toCon wrap ns (dt, vs) m i (NormalC cn []) =
    clause
      [wrap $ lrP m i $ conP 'C [conP 'U []]]
      (normalB $ conE cn) []
toCon wrap ns (dt, vs) m i (NormalC cn fs) =
    -- runIO (putStrLn ("constructor " ++ show ix)) >>
    clause
      [wrap $ lrP m i $ conP 'C
        [foldr1 prod (zipWith (toField (dt, vs)) [0..] (map snd fs))]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where prod x y = conP '(:*:) [x,y]
toCon wrap ns (dt, vs) m i r@(RecC cn []) =
    clause
      [wrap $ lrP m i $ conP 'U []]
      (normalB $ conE cn) []
toCon wrap ns (dt, vs) m i r@(RecC cn fs) =
    clause
      [wrap $ lrP m i $ conP 'C
        [foldr1 prod (zipWith (toField (dt, vs)) [0..] (map trd fs))]]
      (normalB $ foldl appE (conE cn) (map (varE . field) [0..length fs - 1])) []
  where prod x y = conP '(:*:) [x,y]
toCon wrap ns (dt, vs) m i (InfixC t1 cn t2) =
  toCon wrap ns (dt, vs) m i (NormalC cn [t1,t2])

toField :: (Name, [Name]) -> Int -> Type -> Q Pat
--toField (dt, vs) nr t | t == dataDeclToType (dt, vs) = conP 'I [varP (field nr)]
toField (dt, vs) nr t = conP 'Rec [varP (field nr)]


field :: Int -> Name
field n = mkName $ "f" ++ show n

lrP :: Int -> Int -> (Q Pat -> Q Pat)
lrP 1 0 p = p
lrP m 0 p = conP 'L [p]
lrP m i p = conP 'R [lrP (m-1) (i-1) p]

lrE :: Int -> Int -> (Q Exp -> Q Exp)
lrE 1 0 e = e
lrE m 0 e = conE 'L `appE` e
lrE m i e = conE 'R `appE` lrE (m-1) (i-1) e

trd (_,_,c) = c

-- | Variant of foldr1 which returns a special element for empty lists
foldr1' f x [] = x
foldr1' _ _ [x] = x
foldr1' f x (h:t) = f h (foldr1' f x t)
