{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Analysis.Domain
  ( DataDomain (..)
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Int
import GHC.Generics
import Prelude (Show)

data DataDomain a
  = DataDomain
    { ddRawFirst      :: !a
    , ddRawLast       :: !a
    , ddFilteredFirst :: !a
    , ddFilteredLast  :: !a
    , ddRawCount      :: Int
    , ddFilteredCount :: Int
    }
  deriving (Generic, Show, ToJSON, FromJSON)
  deriving anyclass NFData
-- Perhaps:  Plutus.V1.Ledger.Slot.SlotRange = Interval Slot
