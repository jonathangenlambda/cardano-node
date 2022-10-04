{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Validate
  ( TxAuxScriptsValidationError(..)
  , TxFeeValidationError(..)
  , TxValidityLowerBoundValidationError(..)
  , TxValidityUpperBoundValidationError(..)
  , TxReturnCollateralValidationError(..)
  , TxTotalCollateralValidationError(..)
  , validateScriptSupportedInEra
  , validateTxAuxScripts
  , validateTxFee
  , validateRequiredSigners
  , validateTxReturnCollateral
  , validateTxScriptValidity
  , validateTxTotalCollateral
  , validateTxValidityUpperBound
  , validateTxValidityLowerBound
  ) where

import           Prelude

import qualified Data.Map as Map
import           Data.Maybe

import           Cardano.Api
import           Cardano.Api.Shelley
--TODO: following import needed for orphan Eq Script instance
import           Cardano.Ledger.Shelley.Scripts ()

data ScriptLanguageValidationError
  = ScriptLanguageValidationError AnyScriptLanguage AnyCardanoEra

validateScriptSupportedInEra
  :: CardanoEra era
  -> ScriptInAnyLang
  -> Either ScriptLanguageValidationError (ScriptInEra era)
validateScriptSupportedInEra era script@(ScriptInAnyLang lang _) =
  case toScriptInEra era script of
    Nothing -> Left $ ScriptLanguageValidationError
                        (AnyScriptLanguage lang) (anyCardanoEra era)
    Just script' -> pure script'


data TxFeeValidationError
  = TxFeatureImplicitFeesE -- ^ Expected an explicit fee
  | TxFeatureExplicitFeesE -- ^ Expected an implicit fee

validateTxFee :: CardanoEra era
              -> Maybe Lovelace
              -> Either TxFeeValidationError (TxFee era)
validateTxFee era mfee =
    case (txFeesExplicitInEra era, mfee) of
      (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
      (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)

      (Right _, Nothing) -> Left TxFeatureImplicitFeesE
      (Left  _, Just _)  -> Left TxFeatureExplicitFeesE

data TxTotalCollateralValidationError
  = TxTotalCollateralNotSupported AnyCardanoEra

validateTxTotalCollateral :: CardanoEra era
                          -> Maybe Lovelace
                          -> Either TxTotalCollateralValidationError (TxTotalCollateral era)
validateTxTotalCollateral _ Nothing = return TxTotalCollateralNone
validateTxTotalCollateral era (Just coll) =
  case totalAndReturnCollateralSupportedInEra era of
    Just supp -> return $ TxTotalCollateral supp coll
    Nothing -> Left $ TxTotalCollateralNotSupported $ AnyCardanoEra era

data TxReturnCollateralValidationError
  = TxReturnCollateralNotSupported AnyCardanoEra

validateTxReturnCollateral :: CardanoEra era
                           -> Maybe (TxOut CtxTx era)
                           -> Either TxReturnCollateralValidationError (TxReturnCollateral CtxTx era)
validateTxReturnCollateral _ Nothing = return TxReturnCollateralNone
validateTxReturnCollateral era (Just retColTxOut) = do
  case totalAndReturnCollateralSupportedInEra era of
    Just supp -> return $ TxReturnCollateral supp retColTxOut
    Nothing -> Left $ TxReturnCollateralNotSupported $ AnyCardanoEra era

data TxValidityLowerBoundValidationError
  = TxValidityLowerBoundNotSupported AnyCardanoEra

validateTxValidityLowerBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> Either TxValidityLowerBoundValidationError (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound era (Just slot) =
    case validityLowerBoundSupportedInEra era of
      Nothing -> Left $ TxValidityLowerBoundNotSupported $ AnyCardanoEra era
      Just supported -> return (TxValidityLowerBound supported slot)

data TxValidityUpperBoundValidationError
  = TxValidityUpperBoundNotSupported AnyCardanoEra

validateTxValidityUpperBound
  :: CardanoEra era
  -> Maybe SlotNo
  -> Either TxValidityUpperBoundValidationError (TxValidityUpperBound era)
validateTxValidityUpperBound era Nothing =
  case validityNoUpperBoundSupportedInEra era of
    Nothing -> Left $ TxValidityUpperBoundNotSupported $ AnyCardanoEra era
    Just supported -> return (TxValidityNoUpperBound supported)
validateTxValidityUpperBound era (Just slot) =
  case validityUpperBoundSupportedInEra era of
    Nothing -> Left $ TxValidityUpperBoundNotSupported $ AnyCardanoEra era
    Just supported -> return (TxValidityUpperBound supported slot)

data TxAuxScriptsValidationError
  = TxAuxScriptsNotSupported AnyCardanoEra

validateTxAuxScripts
  :: CardanoEra era
  -> [ScriptInAnyLang]
  -> Either TxAuxScriptsValidationError (TxAuxScripts era)
validateTxAuxScripts _ [] = return TxAuxScriptsNone
validateTxAuxScripts era scripts =
  case auxScriptsSupportedInEra era of
    Nothing -> Left $ TxAuxScriptsNotSupported $ AnyCardanoEra era
    Just supported -> do
      scriptsInEra <- mapM (validateScriptSupportedInEra era) scripts
      return $ TxAuxScripts supported scriptsInEra

data RequiredSignersValidationError
  = RequiredSignersValidationError AnyCardanoEra

validateRequiredSigners
  :: CardanoEra era
  -> [Hash PaymentKey]
  -> Either RequiredSignersValidationError (TxExtraKeyWitnesses era)
validateRequiredSigners _ [] = return TxExtraKeyWitnessesNone
validateRequiredSigners era reqSigs =
  case extraKeyWitnessesSupportedInEra era of
    Nothing -> Left $ RequiredSignersValidationError $ AnyCardanoEra era
    Just supported -> return $ TxExtraKeyWitnesses supported reqSigs

data TxWithdrawalsValidationError
  = TxWithdrawalsNotSupported AnyCardanoEra

validateTxWithdrawals
  :: forall era.
     CardanoEra era
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))]
  -> Either TxWithdrawalsValidationError (TxWithdrawals BuildTx era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals =
  case withdrawalsSupportedInEra era of
    Nothing -> Left $ TxWithdrawalsNotSupported $ AnyCardanoEra era
    Just supported -> do
      let convWithdrawals = map convert withdrawals
      return (TxWithdrawals supported convWithdrawals)
 where
  convert
    :: (StakeAddress, Lovelace, Maybe (ScriptWitness WitCtxStake era))
    -> (StakeAddress, Lovelace, BuildTxWith BuildTx (Witness WitCtxStake era))
  convert (sAddr, ll, mScriptWitnessFiles) =
    case mScriptWitnessFiles of
      Just sWit -> do
        (sAddr, ll, BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit)
      Nothing -> (sAddr,ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

data TxCertificatesValidationError
  = TxCertificatesValidationNotSupported AnyCardanoEra

validateTxCertificates
  :: forall era.
     CardanoEra era
  -> [(Certificate, Maybe (ScriptWitness WitCtxStake era))]
  -> Either TxCertificatesValidationError (TxCertificates BuildTx era)
validateTxCertificates _ [] = return TxCertificatesNone
validateTxCertificates era certsAndScriptWitnesses =
  case certificatesSupportedInEra era of
    Nothing -> Left $ TxCertificatesValidationNotSupported $ AnyCardanoEra era
    Just supported -> do
      let certs = map fst certsAndScriptWitnesses
          reqWits = Map.fromList $ mapMaybe convert certsAndScriptWitnesses
      return $ TxCertificates supported certs $ BuildTxWith reqWits
  where
   -- We get the stake credential witness for a certificate that requires it.
   -- NB: Only stake address deregistration and delegation requires
   -- witnessing (witness can be script or key)
   deriveStakeCredentialWitness
     :: Certificate
     -> Maybe StakeCredential
   deriveStakeCredentialWitness cert = do
     case cert of
       StakeAddressDeregistrationCertificate sCred -> Just sCred
       StakeAddressDelegationCertificate sCred _ -> Just sCred
       _ -> Nothing

   convert
     :: (Certificate, Maybe (ScriptWitness WitCtxStake era))
     -> Maybe (StakeCredential, Witness WitCtxStake era)
   convert (cert, mScriptWitnessFiles) = do
     sCred <- deriveStakeCredentialWitness cert
     case mScriptWitnessFiles of
       Just sWit -> do
         Just ( sCred
              , ScriptWitness ScriptWitnessForStakeAddr sWit
              )
       Nothing -> Just (sCred, KeyWitness KeyWitnessForStakeAddr)

data ProtocolParametersValidationError
  = ProtocolParametersNotSupported AnyCardanoEra

validateProtocolParameters
  :: CardanoEra era
  -> Maybe ProtocolParameters
  -> Either ProtocolParametersValidationError (BuildTxWith BuildTx (Maybe ProtocolParameters))
validateProtocolParameters _ Nothing = return (BuildTxWith Nothing)
validateProtocolParameters era (Just pparams) =
    case scriptDataSupportedInEra era of
      Nothing -> Left $ ProtocolParametersNotSupported $ AnyCardanoEra era
      Just _  -> return . BuildTxWith $ Just pparams

data TxUpdateProposalValidationError
  = TxUpdateProposalNotSupported AnyCardanoEra

validateTxUpdateProposal
  :: CardanoEra era
  -> Maybe UpdateProposal
  -> Either TxUpdateProposalValidationError (TxUpdateProposal era)
validateTxUpdateProposal _ Nothing = return TxUpdateProposalNone
validateTxUpdateProposal era (Just prop) =
    case updateProposalSupportedInEra era of
      Nothing -> Left $ TxUpdateProposalNotSupported $ AnyCardanoEra era
      Just supported -> return $ TxUpdateProposal supported prop

data ScriptValidityValidationError
  = ScriptValidityNotSupported AnyCardanoEra

validateTxScriptValidity
  :: CardanoEra era
  -> Maybe ScriptValidity
  -> Either ScriptValidityValidationError (TxScriptValidity era)
validateTxScriptValidity _ Nothing = pure TxScriptValidityNone
validateTxScriptValidity era (Just scriptValidity) =
  case txScriptValiditySupportedInCardanoEra era of
    Nothing -> Left $ ScriptValidityNotSupported $ AnyCardanoEra era
    Just supported -> pure $ TxScriptValidity supported scriptValidity
