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

module CheckSameInlineDatumAtMultipleInputs
  ( printRedeemer,
    serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
  )
where

import           Cardano.Api                          (writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       PlutusScriptV2,
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
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import           Plutus.V2.Ledger.Tx
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       print, (.))

{-
   Expected inline datum to use in redeemer
-}

myDatum = PlutusV2.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData (42 :: Integer)

{-
   Redeemer
-}

redeemer =  [
                PlutusV2.TxOutRef {txOutRefId = "2b1a7a149c1a3574f5d0c5afda47a4fef7c03df69a41551465503ffb6eddc996", txOutRefIdx = 1} ,
                PlutusV2.TxOutRef {txOutRefId = "2b1a7a149c1a3574f5d0c5afda47a4fef7c03df69a41551465503ffb6eddc996", txOutRefIdx = 2}
            ]

printRedeemer = print $ "Script Data: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)

{-
   The validator script
-}

{-# INLINEABLE expectedInlinePolicy #-}
expectedInlinePolicy :: PlutusV2.Datum -> [PlutusV2.TxOutRef] -> PlutusV2.ScriptContext -> Bool
expectedInlinePolicy d refs ctx = traceIfFalse "Unexpected inline datum at each regular input"   (P.all (P.== True) (map (txoDatumIs42 . findTxIn)    refs)) &&
                                  traceIfFalse "Unexpected inline datum at each reference input" (P.all (P.== True) (map (txoDatumIs42 . findRefTxIn) refs))
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        fromJust' :: BuiltinString -> Maybe a -> a -- should be built-in
        fromJust' err Nothing = traceError err
        fromJust' _ (Just x)  = x

        findTxIn :: PlutusV2.TxOutRef -> PlutusV2.TxInInfo
        findTxIn r = fromJust' "txIn doesn't exist" $ PlutusV2.findTxInByTxOutRef r info

        findRefTxInByTxOutRef :: TxOutRef -> PlutusV2.TxInfo -> Maybe PlutusV2.TxInInfo -- similar to findTxInByTxOutRef, should be a built-in context
        findRefTxInByTxOutRef outRef PlutusV2.TxInfo{txInfoReferenceInputs} = find (\PlutusV2.TxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoReferenceInputs

        findRefTxIn :: PlutusV2.TxOutRef -> PlutusV2.TxInInfo
        findRefTxIn r = fromJust' "txRefIn doesn't exist" $ findRefTxInByTxOutRef r info

        txoDatumIs42 :: PlutusV2.TxInInfo -> Bool
        txoDatumIs42 txin = PlutusV2.OutputDatum d P.== PlutusV2.txOutDatum (PlutusV2.txInInfoResolved txin)

{-
    As a Minting Policy
-}

policy :: PlutusV2.Datum -> Scripts.MintingPolicy
policy d = PlutusV2.mkMintingPolicyScript $
        $$(PlutusTx.compile [||PSU.V2.mkUntypedMintingPolicy . expectedInlinePolicy||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode d
{-
    As a Script
-}

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript $ policy myDatum
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
writeSerialisedScript = void $ writeFileTextEnvelope "check-same-inline-datum-at-multiple-inputs.plutus" Nothing serialisedScript
