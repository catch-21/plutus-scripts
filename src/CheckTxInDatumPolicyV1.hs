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

module CheckTxInDatumPolicyV1
  ( printScriptHash,
    printScriptData,
    myDatum,
    redeemerDatumHash,
    redeemerNothing,
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
import qualified Plutus.Script.Utils.Scripts          as PSU
import qualified Plutus.Script.Utils.V1.Scripts       as PSU.V1
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import qualified Plutus.V1.Ledger.Scripts             as PlutusV1
import           Plutus.V1.Ledger.Tx
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       Show (..), print, (.))

{-
   Define redeemer type to handle expected inline datum or datum hash at a txo
-}

data ExpTxInDatumHash = ExpTxInDatumHash
        { txOutRef     :: PlutusV1.TxOutRef,
          expDatumHash :: Maybe PlutusV1.DatumHash
        }
    deriving (Show)

PlutusTx.unstableMakeIsData ''ExpTxInDatumHash

{-
   Expected inline datum to use in redeemer
-}

data SomeData = SomeData {name :: BuiltinByteString, age :: Integer, shopping :: [BuiltinByteString]}

PlutusTx.unstableMakeIsData ''SomeData

someData = SomeData {name = "cats", age = 42, shopping = ["apple", "tomato", "cheese"]}

fortyTwo = 42 :: Integer

devil = 666 :: Integer

text :: BuiltinByteString
text = "check_ref_inputs"

myDatum = PlutusV1.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData fortyTwo

myDatumHash = PSU.datumHash myDatum

{-
   Redeemers
-}

redeemerDatumHash = ExpTxInDatumHash { txOutRef  = PlutusV1.TxOutRef {txOutRefId = "2b1a7a149c1a3574f5d0c5afda47a4fef7c03df69a41551465503ffb6eddc996", txOutRefIdx = 2}
                                     , expDatumHash  = Just myDatumHash
                                     }

redeemerNothing = ExpTxInDatumHash   { txOutRef  = PlutusV1.TxOutRef {txOutRefId = "2b1a7a149c1a3574f5d0c5afda47a4fef7c03df69a41551465503ffb6eddc996", txOutRefIdx = 2}
                                     , expDatumHash  = Nothing
                                     }


printScriptData d = print $ "Script Data: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV1.toData d)

{-
   The validator script
-}

{-# INLINEABLE expectedInlinePolicy #-}
expectedInlinePolicy :: ExpTxInDatumHash -> PlutusV1.ScriptContext -> Bool
expectedInlinePolicy expInline ctx = traceIfFalse "Expected datumhash is not in txin" $ expDatumHash expInline == PlutusV1.txOutDatumHash (PlutusV1.txInInfoResolved findTxIn)
    where
        info :: PlutusV1.TxInfo
        info = PlutusV1.scriptContextTxInfo ctx

        fromJust' :: BuiltinString -> Maybe a -> a -- should be built-in
        fromJust' err Nothing = traceError err
        fromJust' _ (Just x)  = x

        findTxIn :: PlutusV1.TxInInfo
        findTxIn = fromJust' "txIn doesn't exist" $ PlutusV1.findTxInByTxOutRef (txOutRef expInline) info

{-
    As a Minting Policy
-}

compiledCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
compiledCode = $$(PlutusTx.compile [|| wrap ||])
     where
         wrap = PSU.V1.mkUntypedMintingPolicy expectedInlinePolicy

policy :: Scripts.MintingPolicy
policy = PlutusV1.mkMintingPolicyScript compiledCode

thisScriptHash :: PlutusV1.ScriptHash
thisScriptHash = PSU.V1.scriptHash $ PlutusV1.fromCompiledCode compiledCode

printScriptHash = print $ "Script Hash: " ++ show thisScriptHash

{-
    As a Script
-}

script :: PlutusV1.Script
script = PlutusV1.unMintingPolicyScript policy

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
writeSerialisedScript = void $ writeFileTextEnvelope "check-tx-in-datum-hash-V1.plutus" Nothing serialisedScript
