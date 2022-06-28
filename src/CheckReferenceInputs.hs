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
    serialisedScript,
    scriptSBS,
    writeSerialisedScript,
  )
where

import           Cardano.Api                    (PlutusScriptV2,
                                                 writeFileTextEnvelope)
import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                 fromPlutusData,
                                                 scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                     as A
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           Data.Functor                   (void)
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude               as P hiding (Semigroup (..),
                                                      unless, (.))
import           Prelude                        (IO, Semigroup (..), print, (.))

{-
   Redeemers
-}

redeemer :: [ PlutusV2.TxOutRef ]
redeemer =  [ PlutusV2.TxOutRef "b204b4554a827178b48275629e5eac9bde4f5350badecfcd108d87446f00bf26" 0,
              PlutusV2.TxOutRef "b204b4554a827178b48275629e5eac9bde4f5350badecfcd108d87446f00bf26" 1 ]

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)

{-
   The validator script
-}

{-# INLINEABLE expectedInlinePolicy #-}
expectedInlinePolicy :: BuiltinData -> [PlutusV2.TxOutRef] -> PlutusV2.ScriptContext -> Bool
expectedInlinePolicy _ expRefTxos ctx =  traceIfFalse "Reference inputs do not much redeemer" $ expRefTxos == refInputdatums
    where
        info = PlutusV2.scriptContextTxInfo ctx
        refInputdatums = map PlutusV2.txInInfoOutRef $ PlutusV2.txInfoReferenceInputs info

{-
    As a validator
-}

policy :: Scripts.Validator
policy = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
     where
         wrap = PSU.V2.mkUntypedValidator expectedInlinePolicy

{-
   As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise $ policy

{-
   As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "check-reference-inputs.plutus" Nothing serialisedScript

