{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module CheckRedeemersPolicy
  ( printRedeemer,
    serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript
  )
where

import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import           Plutus.V2.Ledger.Contexts            (ownCurrencySymbol)
import qualified PlutusTx
import qualified PlutusTx.AssocMap                    as AMap
import qualified PlutusTx.Builtins                    as BI
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       print, (.))

{-
   Redeemers
-}

--data ExpRedeemers = ExpRedeemers {redeemers :: [Plutus.Redeemer]}

--PlutusTx.unstableMakeIsData ''ExpRedeemers

asRedeemer :: PlutusTx.ToData a => a -> PlutusV2.Redeemer
asRedeemer a = PlutusV2.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

intAsRedeemer :: Integer -> PlutusV2.Redeemer
intAsRedeemer = asRedeemer @Integer

redeemer :: [PlutusV2.Redeemer]
redeemer =  [intAsRedeemer 42, intAsRedeemer 43, asRedeemer @BI.BuiltinByteString "d"]

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)

{-
   The validator script
-}

{-# INLINEABLE checkRedeemersPolicy #-}
checkRedeemersPolicy :: [PlutusV2.Redeemer] -> PlutusV2.ScriptContext -> Bool
checkRedeemersPolicy expRedeemers ctx =  traceIfFalse "Redeemers do not match expected" $ P.all ((P.== True) . findR) expRedeemers            &&
                                         traceIfFalse "Number of redeemers (without own) does not match expected" (P.length expRedeemers P.== P.length withoutOwnRedeemer)
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        thisScriptPurpose :: PlutusV2.ScriptPurpose
        thisScriptPurpose = PlutusV2.Minting $ ownCurrencySymbol ctx

        withoutOwnRedeemer :: [PlutusV2.Redeemer]
        withoutOwnRedeemer = AMap.elems $ AMap.delete thisScriptPurpose (PlutusV2.txInfoRedeemers info)

        findR :: PlutusV2.Redeemer -> Bool
        findR r = P.isJust $ P.find (P.== r) withoutOwnRedeemer
{-
    As a Minting Policy
-}

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy checkRedeemersPolicy

{-
    As a Script
-}

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript policy

{-
    As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

{-
    As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "check-redeemers-policy.plutus" Nothing serialisedScript
