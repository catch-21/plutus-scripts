{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module ValidRangeEquivilance
  ( serialisedScriptV1,
    scriptSBSV1,
    scriptV1,
    writeSerialisedScriptV1,
    serialisedScriptV2,
    scriptSBSV2,
    scriptV2,
    writeSerialisedScriptV2,
    printScriptDataV1,
    printScriptDataV2
  ) where

import           Cardano.Api                          (PlutusScriptV1,
                                                       PlutusScriptV2,
                                                       ScriptData,
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       scriptDataToJson,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       fromPlutusData)
import           Codec.Serialise
import qualified Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Data.Text.Internal.Fusion.Size       (upperBound)
import           Ledger                               (Interval (ivTo),
                                                       ScriptPurpose)
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Api                 as V2PlutusV2
import qualified Plutus.V1.Ledger.Interval            as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import           PlutusTx.AssocMap                    (Map)
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup ((<>)),
                                                       Show, print, (.))

data PV1CustomRedeemer
  = PV1CustomRedeemer
      {
        pv1Inputs      :: [PlutusV1.TxInInfo]
      , pv1Outputs     :: [PlutusV1.TxOut]
      , pv1Fee         :: PlutusV1.Value
      , pv1Mint        :: PlutusV1.Value
      , pv1DCert       :: [PlutusV1.DCert]
      , pv1Wdrl        :: Map PlutusV1.StakingCredential Integer
      , pv1ValidRange  :: PlutusV1.POSIXTimeRange
      , pv1Signatories :: [PlutusV1.PubKeyHash]
      , pv1Data        :: Map PlutusV1.DatumHash PlutusV1.Datum
      } deriving (Eq, Show)

PlutusTx.unstableMakeIsData ''PV1CustomRedeemer

data PV2CustomRedeemer
  = PV2CustomRedeemer
      {
        pv2Inputs       :: [PlutusV2.TxInInfo]
--      , pv2RefInputs   :: [PlutusV2.TxInInfo]
--      , pv2Outputs     :: [PlutusV2.TxOut]
--      , pv2Fee         :: PlutusV2.Value
--      , pv2Mint        :: PlutusV2.Value
--      , pv2DCert       :: [PlutusV2.DCert]
--      , pv2Wdrl        :: PlutusV2.Map V2PlutusV2.StakingCredential Integer
        , pv2ValidRange :: PlutusV2.POSIXTimeRange
--      , pv2Signatories :: [PlutusV2.PubKeyHash]
--      , pv2Redeemers   :: PlutusV2.Map ScriptPurpose PlutusV2.Redeemer
--      , pv2Data        :: PlutusV2.Map PlutusV2.DatumHash PlutusV2.Datum
      } deriving (Eq, Show)

PlutusTx.unstableMakeIsData ''PV2CustomRedeemer

data AnyCustomRedeemer
  = AnyPV1CustomRedeemer PV1CustomRedeemer |
    AnyPV2CustomRedeemer PV2CustomRedeemer
  deriving (Show, Eq)

--PlutusTx.unstableMakeIsData ''AnyCustomRedeemer

customRedeemerToScriptData :: AnyCustomRedeemer -> ScriptData
customRedeemerToScriptData (AnyPV1CustomRedeemer cRedeem) =
  fromPlutusData $ PlutusV1.toData cRedeem
customRedeemerToScriptData (AnyPV2CustomRedeemer cRedeem) =
  fromPlutusData $ PlutusV2.toData cRedeem

timeRangeV1 :: PlutusV1.POSIXTimeRange
timeRangeV1 = PlutusV1.Interval
  (PlutusV1.LowerBound (PlutusV1.Finite $ PlutusV1.POSIXTime 665000000) True)
  (PlutusV1.UpperBound (PlutusV1.Finite $ PlutusV1.POSIXTime 666000000) True)

timeRangeV2 :: PlutusV2.POSIXTimeRange
timeRangeV2 = PlutusV2.Interval
  (PlutusV2.LowerBound (PlutusV2.NegInf) True)
  (PlutusV2.UpperBound (PlutusV2.Finite $ PlutusV2.POSIXTime 666000000) True)



redeemerV2 = PV2CustomRedeemer { pv2Inputs = [], pv2ValidRange = timeRangeV2 }

printScriptDataV1 = print $ "Script Data: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV1.toData timeRangeV1)
printScriptDataV2 = print $ "Script Data: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemerV2)

-- V1

{-# INLINABLE mkPolicyV1 #-}
mkPolicyV1 :: PV2CustomRedeemer -> PlutusV1.ScriptContext -> Bool
mkPolicyV1 (PV2CustomRedeemer _ r) ctx = r /= PlutusV1.txInfoValidRange (PlutusV1.scriptContextTxInfo ctx)
--  where
--    info :: PlutusV1.TxInfo
--    info = PlutusV1.scriptContextTxInfo ctx

--    extendUpperBound :: PlutusV1.POSIXTimeRange -> PlutusV1.POSIXTimeRange
--    extendUpperBound (PlutusV1.Interval l (PlutusV1.UpperBound (PlutusV1.Finite f) c)) = PlutusV1.Interval l (PlutusV1.UpperBound (PlutusV1.Finite ( f + t )) c)

policyV1 :: Scripts.MintingPolicy
policyV1 = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = PSU.V1.mkUntypedMintingPolicy mkPolicyV1

scriptV1 :: PlutusV1.Script
scriptV1 = PlutusV1.unMintingPolicyScript policyV1

scriptSBSV1 :: SBS.ShortByteString
scriptSBSV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

serialisedScriptV1 :: PlutusScript PlutusScriptV1
serialisedScriptV1 = PlutusScriptSerialised scriptSBSV1

writeSerialisedScriptV1 :: IO ()
writeSerialisedScriptV1 = void $ writeFileTextEnvelope "valid-range-inequivilance-v1.plutus" Nothing serialisedScriptV1

-- V2

{-# INLINABLE mkPolicyV2 #-}
mkPolicyV2 :: PV2CustomRedeemer -> PlutusV2.ScriptContext -> Bool
mkPolicyV2 (PV2CustomRedeemer _ r) _ = PlutusV2.ivFrom r == PlutusV2.ivFrom r
--  where
--    info :: PlutusV2.TxInfo
--    info = PlutusV2.scriptContextTxInfo ctx

--    extendUpperBound :: PlutusV2.POSIXTimeRange -> PlutusV2.POSIXTime -> PlutusV2.POSIXTimeRange
--    extendUpperBound (PlutusV2.Interval l (PlutusV2.UpperBound (PlutusV2.Finite f) c)) a = PlutusV2.Interval l (PlutusV2.UpperBound (PlutusV2.Finite $ f + a) c)

policyV2 :: Scripts.MintingPolicy
policyV2 = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = PSU.V2.mkUntypedMintingPolicy mkPolicyV2

scriptV2 :: PlutusV1.Script
scriptV2 = PlutusV1.unMintingPolicyScript policyV2

scriptSBSV2 :: SBS.ShortByteString
scriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

serialisedScriptV2 :: PlutusScript PlutusScriptV2
serialisedScriptV2 = PlutusScriptSerialised scriptSBSV2

writeSerialisedScriptV2 :: IO ()
writeSerialisedScriptV2 = void $ writeFileTextEnvelope "valid-range-equivilance-v2.plutus" Nothing serialisedScriptV2
