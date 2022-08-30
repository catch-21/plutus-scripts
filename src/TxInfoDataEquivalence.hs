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

module TxInfoDataEquivalence
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
import qualified Plutus.Script.Utils.Scripts          as PSU
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
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

datum :: PlutusV2.Datum
datum = intAsDatum 123

datumHash :: PlutusV2.DatumHash
datumHash = PSU.datumHash datum

redeemer :: PlutusV2.Map PlutusV2.DatumHash PlutusV2.Datum
redeemer =  PlutusV2.fromList [(datumHash, datum)]

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)

{-
   The validator script
-}

{-# INLINEABLE checkDatumsPolicy #-}
checkDatumsPolicy :: PlutusV2.Map PlutusV2.DatumHash PlutusV2.Datum -> PlutusV2.ScriptContext -> Bool
checkDatumsPolicy expDatumMap ctx =  traceIfFalse "Datums and hashes in txInfoData do not match expected" (expDatumMap == PlutusV2.txInfoData info)
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx
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
writeSerialisedScript = void $ writeFileTextEnvelope "check-datum-hash-map-policy.plutus" Nothing serialisedScript

