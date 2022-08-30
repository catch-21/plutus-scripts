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

module DeadlineRedeemerPolicy
  ( serialisedScriptV1,
    scriptSBSV1,
    scriptV1,
    writeSerialisedScriptV1,
    serialisedScriptV2,
    scriptSBSV2,
    scriptV2,
    writeSerialisedScriptV2
  ) where

import           Cardano.Api                          (PlutusScriptV1,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..))
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Interval            as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, (.))

-- V1

{-# INLINABLE mkPolicyV1 #-}
mkPolicyV1 :: PlutusV1.POSIXTime -> PlutusV1.ScriptContext -> Bool
mkPolicyV1 dl ctx = PlutusV1.to dl `PlutusV1.contains` range -- transaction's valid range must be before deadline
  where
    info :: PlutusV1.TxInfo
    info = PlutusV1.scriptContextTxInfo ctx

    range :: PlutusV1.POSIXTimeRange
    range = PlutusV1.txInfoValidRange info

policyV1 :: Scripts.MintingPolicy
policyV1 = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy mkPolicyV1 ||])

scriptV1 :: PlutusV1.Script
scriptV1 = PlutusV1.unMintingPolicyScript policyV1

scriptSBSV1 :: SBS.ShortByteString
scriptSBSV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

serialisedScriptV1 :: PlutusScript PlutusScriptV1
serialisedScriptV1 = PlutusScriptSerialised scriptSBSV1

writeSerialisedScriptV1 :: IO ()
writeSerialisedScriptV1 = void $ writeFileTextEnvelope "deadline-redeemer-policy-v1.plutus" Nothing serialisedScriptV1

-- V2

{-# INLINABLE mkPolicyV2 #-}
mkPolicyV2 :: PlutusV2.POSIXTime -> PlutusV2.ScriptContext -> Bool
mkPolicyV2 dl ctx = PlutusV2.to dl `PlutusV1.contains` range -- there's no Plutus.V2.Ledger.Interval
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    range :: PlutusV2.POSIXTimeRange
    range = PlutusV2.txInfoValidRange info

policyV2 :: Scripts.MintingPolicy
policyV2 = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V2.mkUntypedMintingPolicy mkPolicyV2 ||])

scriptV2 :: PlutusV1.Script
scriptV2 = PlutusV1.unMintingPolicyScript policyV2

scriptSBSV2 :: SBS.ShortByteString
scriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

serialisedScriptV2 :: PlutusScript PlutusScriptV2
serialisedScriptV2 = PlutusScriptSerialised scriptSBSV2

writeSerialisedScriptV2 :: IO ()
writeSerialisedScriptV2 = void $ writeFileTextEnvelope "deadline-redeemer-policy-v2.plutus" Nothing serialisedScriptV2
