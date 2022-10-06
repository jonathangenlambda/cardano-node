{-# LANGUAGE DeriveAnyClass #-}
module Data.DataDomain (module Data.DataDomain) where

import Cardano.Prelude
import Data.Aeson (FromJSON, ToJSON)

data DataDomain a
  = DataDomain
    { ddRawFirst      :: !a
    , ddRawLast       :: !a
    , ddFilteredFirst :: !(Maybe a)
    , ddFilteredLast  :: !(Maybe a)
    , ddRawCount      :: Int
    , ddFilteredCount :: Int
    }
  deriving (Generic, Show, ToJSON, FromJSON)
  deriving anyclass NFData
-- Perhaps:  Plutus.V1.Ledger.Slot.SlotRange = Interval Slot

mkDataDomainInj :: a -> a -> (a -> Int) -> DataDomain a
mkDataDomainInj f l measure = DataDomain f l (Just f) (Just l) delta delta
  where delta = measure l - measure f

mkDataDomain :: a -> a -> a -> a -> (a -> Int) -> DataDomain a
mkDataDomain f l f' l' measure =
  DataDomain f l (Just f') (Just l') (measure l - measure f) (measure l' - measure f')

dataDomainsMergeInner :: Ord a => [DataDomain a] -> DataDomain a
dataDomainsMergeInner xs =
  DataDomain
  { ddRawFirst      = maximum $ xs <&> ddRawFirst
  , ddRawLast       = minimum $ xs <&> ddRawLast
  , ddFilteredFirst = bool (Just . maximum $ xs & mapMaybe ddFilteredFirst) Nothing (null xs)
  , ddFilteredLast  = bool (Just . maximum $ xs & mapMaybe ddFilteredLast)  Nothing (null xs)
  , ddRawCount      =     sum $ xs <&> ddRawCount
  , ddFilteredCount =     sum $ xs <&> ddFilteredCount
  }

dataDomainsMergeOuter :: Ord a => [DataDomain a] -> DataDomain a
dataDomainsMergeOuter xs =
  DataDomain
  { ddRawFirst      = minimum $ xs <&> ddRawFirst
  , ddRawLast       = maximum $ xs <&> ddRawLast
  , ddFilteredFirst = minimum $ xs <&> ddFilteredFirst
  , ddFilteredLast  = maximum $ xs <&> ddFilteredLast
  , ddRawCount      =     sum $ xs <&> ddRawCount
  , ddFilteredCount =     sum $ xs <&> ddFilteredCount
  }
