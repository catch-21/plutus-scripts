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

module CountReferenceInputsPolicy
  ( serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
    --  , runTrace
  )
where

import           Cardano.Api                     (PlutusScript, PlutusScriptV2,
                                                  writeFileTextEnvelope)
import           Cardano.Api.Shelley             (PlutusScript (..),
                                                  ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                  fromPlutusData,
                                                  scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                      as A
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as SBS
import           Data.Functor                    (void)
--import           Ledger
import           Ledger.Ada                      as Ada
import           Ledger.Constraints              as Constraints
import qualified Ledger.Typed.Scripts            as Scripts
import           Ledger.Typed.Scripts.Validators
import           Ledger.Value                    as Value
import           Plutus.Contract                 as Contract
import qualified Plutus.Script.Utils.V2.Scripts  as PSU.V2
import           Plutus.Trace.Emulator           as Emulator
import qualified Plutus.V1.Ledger.Api            as PlutusV1
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap               as AMap
import qualified PlutusTx.Builtins               as BI
import           PlutusTx.Prelude                as P hiding (Semigroup (..),
                                                       unless, (.))
import           Prelude                         (IO, Semigroup (..), Show (..),
                                                  String, print, putStrLn, (.))
import           Wallet.Emulator.Wallet

{-
   The validator script
-}

{-# INLINEABLE countReferenceInputssPolicy #-}
countReferenceInputssPolicy :: Integer -> PlutusV2.ScriptContext -> Bool
countReferenceInputssPolicy n ctx =  traceIfFalse "Number of reference inputs does not match expected" $ n P.== P.length (PlutusV2.txInfoReferenceInputs info)
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

{-
    As a Minting Policy
-}

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy countReferenceInputssPolicy

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
writeSerialisedScript = void $ writeFileTextEnvelope "count-reference-inputs-policy.plutus" Nothing serialisedScript

