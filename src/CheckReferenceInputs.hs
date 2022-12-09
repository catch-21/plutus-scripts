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

module CheckReferenceInputs
  ( printRedeemer,
  printRedeemer2,
    serialisedScript,
    scriptSBS,
    writeSerialisedScript,
  )
where

import           Cardano.Api                          (PlutusScriptV2,
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
import           Data.String
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.V1.Ledger.Tx                  (TxId (getTxId))
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

redeemer :: [(PlutusV2.TxId, Integer)]
redeemer =  [("b204b4554a827178b48275629e5eac9bde4f5350badecfcd108d87446f00bf26", 0),
              ("b204b4554a827178b48275629e5eac9bde4f5350badecfcd108d87446f00bf26", 1)]

r2 :: [PlutusV2.TxOutRef]
r2 = expRefTxos' redeemer

expRefTxos' :: [(PlutusV2.TxId, Integer)] -> [PlutusV2.TxOutRef]
expRefTxos' = map (\(txid,idx)-> PlutusV2.TxOutRef{txOutRefId=txid, txOutRefIdx=idx})

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)
printRedeemer2 = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData r2)

{-
   The validator script
-}

{-# INLINEABLE expectedInlinePolicy #-}
expectedInlinePolicy :: [PlutusV2.TxOutRef] -> PlutusV2.ScriptContext -> Bool
expectedInlinePolicy expRefTxos ctx =  traceIfFalse "Reference inputs do not match redeemer" $ expRefTxos == refInputs
    where
        info = PlutusV2.scriptContextTxInfo ctx
        refInputs = map PlutusV2.txInInfoOutRef $ PlutusV2.txInfoReferenceInputs info

{-
    As a validator
-}

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript
        $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy expectedInlinePolicy||])

{-
   As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise policy

{-
   As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "check-reference-inputs.plutus" Nothing serialisedScript

