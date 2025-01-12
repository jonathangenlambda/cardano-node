{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module  Cardano.TxGenerator.Types
        (module Cardano.TxGenerator.Types)
        where

import           Cardano.Api

import           Cardano.TxGenerator.Fund (Fund)

-- some type aliases to keep compatibility with code in Cardano.Benchmarking
type NumberOfInputsPerTx  = Int
type NumberOfOutputsPerTx = Int
type NumberOfTxs          = Int
type TxAdditionalSize     = Int
type TPSRate              = Double


type TxGenerator era = [Fund] -> [TxOut CtxTx era] -> Either String (Tx era, TxId)

type FundSource m       = m (Either String [Fund])
type FundToStore m      = Fund -> m ()
type FundToStoreList m  = [Fund] -> m ()

data PayWithChange
  = PayExact [Lovelace]
  | PayWithChange Lovelace [Lovelace]


data TxGenTxParams = TxGenTxParams
  { txParamFee        :: !Lovelace              -- ^ Transaction fee, in Lovelace
  , txParamAddTxSize  :: !Int                   -- ^ Extra transaction payload, in bytes -- Note [Tx additional size]
  , txParamInputs     :: !NumberOfInputsPerTx   -- ^ Inputs per transaction
  , txParamOutputs    :: !NumberOfOutputsPerTx  -- ^ Outputs per transaction
  }
  deriving Show

-- defaults taken from: cardano-node/nix/nixos/tx-generator-service.nix
defaultTxGenTxParams :: TxGenTxParams
defaultTxGenTxParams = TxGenTxParams
  { txParamFee        = 10_000_000
  , txParamAddTxSize  = 100
  , txParamInputs     = 2
  , txParamOutputs    = 2
  }


data TxGenConfig = TxGenConfig
  { confMinUtxoValue  :: !Lovelace  -- ^ Minimum value required per UTxO entry
  , confTxsPerSecond  :: !Double    -- ^ Strength of generated workload, in transactions per second
  , confInitCooldown  :: !Double    -- ^ Delay between init and main submissions in seconds
  }
  deriving Show


data TxGenPlutusParams =
    PlutusOn                            -- ^ Generate Txs for a Plutus script with explicit settings
      { plutusScript      :: !FilePath  -- ^ Path to the Plutus script
      , plutusData        :: !Int       -- ^ Data passed to the Plutus script (for now only an int)
      , plutusRedeemer    :: !Int       -- ^ Redeemer data passed to the Plutus script (for now only an int)
      , plutusExecMemory  :: !Int       -- ^ Max. memory available for the Plutus script
      , plutusExecSteps   :: !Int       -- ^ Max. execution steps available for the Plutus script
      }
  | PlutusAuto                          -- ^ Generate Txs for a Plutus script, choosing settings to max out per Tx script budget
      { plutusScript      :: !FilePath  -- ^ Path to the Plutus script
      }
  | PlutusOff                           -- ^ Do not generate Plutus Txs
  deriving Show

newtype TxGenError
  = TxFileError (FileError TextEnvelopeError)
  deriving Show

{-
Note [Tx additional size]
~~~~~~~~~~~~~~~~~~~~~~~~~
This parameter specifies the additional size (in bytes) of a transaction.
Since one transaction is ([input] + [output] + attributes), its size
is defined by its inputs and outputs. We want to have an ability to
increase a transaction's size without increasing the number of inputs or
outputs. Such a big transaction will give us more real-world results
of benchmarking.
Technically, this parameter specifies the size of the attribute we'll
add to the transaction (by default attributes are empty, so if this
parameter is skipped, attributes will remain empty).
-}
