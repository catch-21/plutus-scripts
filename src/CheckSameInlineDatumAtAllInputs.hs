{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module CheckSameInlineDatumAtAllInputs
  ( serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript, printRedeemer
  )
where

import           Data.Aeson                           as A

import           Cardano.Api                          (PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       fromPlutusData,
                                                       scriptDataToJson)
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       print, (.))

{-
   Expected inline datum to use in redeemer
-}

{-# INLINEABLE myDatum #-}
myDatum = PlutusV2.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData (42 :: Integer)

datumHash :: PlutusV2.DatumHash
datumHash = PSU.V2.datumHash myDatum

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData datumHash)

{-
   The validator script
-}

{-# INLINEABLE expectedDatumHashPolicy #-}
expectedDatumHashPolicy :: PlutusV2.DatumHash -> PlutusV2.ScriptContext -> P.Bool
expectedDatumHashPolicy dh ctx = traceIfFalse "Unexpected datum hash at each reference input" (P.all (P.== True) $ P.map checkDatumHash allRefTxIn)
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        allRefTxIn :: [PlutusV2.TxInInfo]
        allRefTxIn =  PlutusV2.txInfoReferenceInputs info

        checkDatumHash :: PlutusV2.TxInInfo -> P.Bool
        checkDatumHash txin = PlutusV2.OutputDatumHash dh P.== PlutusV2.txOutDatum (PlutusV2.txInInfoResolved txin)

{-
    As a Minting Policy
-}

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript
        $$(PlutusTx.compile [||PSU.V2.mkUntypedMintingPolicy expectedDatumHashPolicy||])
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
writeSerialisedScript = void $ writeFileTextEnvelope "check-same-datum-hash-at-all-reference-inputs.plutus" Nothing serialisedScript
