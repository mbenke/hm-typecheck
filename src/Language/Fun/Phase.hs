{-# LANGUAGE DataKinds #-}
module Language.Fun.Phase where

data Phase = UD | Ps | Tc | Sp
data FunPhase (c::Phase) where
    FunUD :: FunUD
    FunPs :: FunPs
    FunTc :: FunTc

type FunUD = FunPhase 'UD -- Undecorated
type FunPs = FunPhase 'Ps -- Parsed
type FunTc = FunPhase 'Tc -- Typechecked
type FunSp = FunPhase 'Sp -- Specialised

data NoExtField = NoExtField
instance Semigroup NoExtField where _ <> _ = NoExtField
instance Monoid NoExtField where mempty = NoExtField

data DataConCantHappen -- empty data type
-- void :: DataConCantHappen
-- void = error "Attempt to evaluate void"

absurd :: DataConCantHappen -> a
absurd v = case v of {}