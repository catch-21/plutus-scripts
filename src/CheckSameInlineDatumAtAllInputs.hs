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
    writeSerialisedScript,
  )
where

import           Cardano.Api                    (writeFileTextEnvelope)
import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           Data.Functor                   (void)
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude               as P hiding (Semigroup (..),
                                                      unless, (.))
import           Prelude                        (IO, (.))

{-
   Expected inline datum to use in redeemer
-}

{-# INLINEABLE myDatum #-}
myDatum = PlutusV2.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData (42 :: Integer)

{-
   The validator script
-}

{-# INLINEABLE expectedInlinePolicy #-}
expectedInlinePolicy :: PlutusV2.Datum -> BuiltinData -> PlutusV2.ScriptContext -> P.Bool
expectedInlinePolicy d _ ctx = traceIfFalse "Unexpected inline datum at each regular input"   (P.all (P.== True) $ P.map checkInlineDatum    allTxIn) &&
                               traceIfFalse "Unexpected inline datum at each reference input" (P.all (P.== True) $ P.map checkInlineDatum allRefTxIn)
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        allTxIn :: [PlutusV2.TxInInfo]
        allTxIn =  PlutusV2.txInfoInputs info

        allRefTxIn :: [PlutusV2.TxInInfo]
        allRefTxIn =  PlutusV2.txInfoReferenceInputs info

        checkInlineDatum :: PlutusV2.TxInInfo -> P.Bool
        checkInlineDatum txin = PlutusV2.OutputDatum d P.== PlutusV2.txOutDatum (PlutusV2.txInInfoResolved txin)

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
writeSerialisedScript = void $ writeFileTextEnvelope "check-same-inline-datum-at-all-inputs.plutus" Nothing serialisedScript
