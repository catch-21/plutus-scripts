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

module CheckReferenceScriptPolicy
  ( policyHash,
    printRedeemer,
    serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
  )
where

import           Cardano.Api                          (PlutusScript (..),
                                                       PlutusScriptV2,
                                                       ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                       scriptDataToJson,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (PlutusScriptSerialised),
                                                       fromPlutusData)
import           Codec.Serialise
import           Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Ledger
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.V2.Ledger.Api                 (toData)
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import           PlutusTx                             (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       Show (..), print, (.))

{-
   Define redeemer type to handle expected inline datum or datum hash at a txo
-}

data InputType = RegularInput | ReferenceInput | BothInputTypes
    deriving (Show)

PlutusTx.unstableMakeIsData ''InputType

data ExpRefScript = ExpRefScript
        { txOutRef     :: TxOutRef,
          expRefScript :: Maybe ScriptHash,
          inputType    :: InputType
        }
    deriving (Show)

PlutusTx.unstableMakeIsData ''ExpRefScript

{-
   Redeemers
-}

redeemer = ExpRefScript { txOutRef  = TxOutRef {txOutRefId = "b204b4554a827178b48275629e5eac9bde4f5350badecfcd108d87446f00bf26", txOutRefIdx = 0}
                        , expRefScript  = Just  policyScriptHash -- "c4a19ee0baedc17a949f902688a6f6752673862ad921d23fb8233e23" <- this policy's script hash
                        , inputType = RegularInput
                        }

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ toData policyScriptHash)

{-
   The validator script
-}

{-# INLINEABLE expectedRefScriptPolicy #-}
expectedRefScriptPolicy :: ExpRefScript -> PlutusV2.ScriptContext -> Bool
expectedRefScriptPolicy expRefScript ctx =
    case expRefScript of
        ExpRefScript _ Nothing RegularInput     -> noReferenceScriptInInput
        ExpRefScript _ Nothing ReferenceInput   -> noReferenceScriptInRefInput
        ExpRefScript _ Nothing BothInputTypes   -> noReferenceScriptInInput && noReferenceScriptInRefInput

        ExpRefScript _ sh@(Just _) RegularInput   -> referenceScriptInInput sh
        ExpRefScript _ sh@(Just _) ReferenceInput -> referenceScriptInRefInput sh
        ExpRefScript _ sh@(Just _) BothInputTypes -> referenceScriptInInput sh && referenceScriptInRefInput sh

        _                                       -> traceError "Unexpected case"
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        fromJust' :: BuiltinString -> Maybe a -> a
        fromJust' err Nothing = traceError err
        fromJust' _ (Just x)  = x

        findTxIn :: PlutusV2.TxInInfo
        findTxIn = fromJust' "txIn doesn't exist" $ PlutusV2.findTxInByTxOutRef (txOutRef expRefScript) info

        findRefTxInByTxOutRef :: TxOutRef -> PlutusV2.TxInfo -> Maybe PlutusV2.TxInInfo -- similar to findTxInByTxOutRef, should be a built-in context
        findRefTxInByTxOutRef outRef PlutusV2.TxInfo{txInfoReferenceInputs} =
            find (\PlutusV2.TxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoReferenceInputs

        findRefTxIn :: PlutusV2.TxInInfo
        findRefTxIn = fromJust' "txRefIn doesn't exist" $ findRefTxInByTxOutRef (txOutRef expRefScript) info

        noReferenceScriptInInput  = traceIfFalse "Expected regular input to have no reference script" $ P.isNothing $ PlutusV2.txOutReferenceScript $ PlutusV2.txInInfoResolved findTxIn
        referenceScriptInInput sh = traceIfFalse "Expected regular input to have reference script"    $ sh == PlutusV2.txOutReferenceScript (PlutusV2.txInInfoResolved findTxIn)

        noReferenceScriptInRefInput  = traceIfFalse "Expected reference input to have no reference script" $ P.isNothing $ PlutusV2.txOutReferenceScript $ PlutusV2.txInInfoResolved findRefTxIn
        referenceScriptInRefInput sh = traceIfFalse "Expected reference input to have reference script"    $ sh == PlutusV2.txOutReferenceScript (PlutusV2.txInInfoResolved findRefTxIn)

{-
    As a Minting Policy
-}

compiledCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
compiledCode = $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy expectedRefScriptPolicy

policyScriptHash :: ScriptHash
policyScriptHash = PSU.V2.scriptHash $ fromCompiledCode compiledCode

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript compiledCode

policyHash :: MintingPolicyHash
policyHash = PSU.V2.mintingPolicyHash policy -- different way to produce the same hash as policyScriptHash

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
writeSerialisedScript = void $ writeFileTextEnvelope "check-reference-script.plutus" Nothing serialisedScript
