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

module CheckDatumMapPolicy
  ( printRedeemer,
    serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
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
import qualified PlutusTx
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

asDatum :: PlutusTx.ToData a => a -> PlutusV2.Datum
asDatum a = PlutusV2.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

intAsDatum :: Integer -> PlutusV2.Datum
intAsDatum = asDatum @Integer

redeemer :: [PlutusV2.Datum]
redeemer =  [intAsDatum 42, intAsDatum 43, asDatum @BI.BuiltinByteString "d"]

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)

{-
   The validator script
-}

{-# INLINEABLE checkDatumsPolicy #-}
checkDatumsPolicy :: [PlutusV2.Datum] -> PlutusV2.ScriptContext -> Bool
checkDatumsPolicy expRedeemers ctx =  traceIfFalse "Datums in txInfoData do not match expected" $ P.all ((P.== True) . findD) expRedeemers
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        findD :: PlutusV2.Datum -> Bool
        findD r = P.isJust $ P.find (P.== r) (PlutusV2.txInfoData info)
{-
    As a Minting Policy
-}

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy checkDatumsPolicy

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
writeSerialisedScript = void $ writeFileTextEnvelope "check-datum-map-policy.plutus" Nothing serialisedScript

