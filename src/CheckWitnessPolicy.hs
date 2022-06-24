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

module CheckWitnessPolicy
  ( serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
    --  , runTrace
  )
where

import           Cardano.Api                         (PlutusScript (..),
                                                      PlutusScriptV2,
                                                      ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                      scriptDataToJson,
                                                      writeFileTextEnvelope)
import           Cardano.Api.Shelley                 (PlutusScript (PlutusScriptSerialised),
                                                      fromPlutusData)
import           Codec.Serialise
import           Data.Aeson                          as A
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import           Data.Functor                        (void)
import qualified Data.Text.Internal.ByteStringCompat as BI
import           Ledger
import           Ledger.Ada                          as Ada
import           Ledger.Constraints                  as Constraints
import qualified Ledger.Typed.Scripts                as Scripts
import           Ledger.Typed.Scripts.Validators
import           Ledger.Value                        as Value
import           Plutus.Contract                     as Contract
import qualified Plutus.Contract                     as Scripts
import           Plutus.Contract.Schema              (Input)
import qualified Plutus.Script.Utils.V2.Scripts      as PSU.V2
import           Plutus.Trace.Emulator               as Emulator
import           Plutus.V2.Ledger.Api                (toData)
import qualified Plutus.V2.Ledger.Api                as PlutusV2
import qualified Plutus.V2.Ledger.Contexts           as PlutusV2
import           PlutusTx                            (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.Builtins                   as BI
import           PlutusTx.Prelude                    as P hiding
                                                          (Semigroup (..),
                                                           unless, (.))
import           Prelude                             (IO, Semigroup (..),
                                                      Show (..), String, print,
                                                      putStrLn, (.))
import           Wallet.Emulator.Wallet

{-
   The policy
-}

{-# INLINEABLE expectedWitnessPolicy #-}
expectedWitnessPolicy :: PlutusV2.PubKeyHash -> PlutusV2.ScriptContext -> Bool
expectedWitnessPolicy pkh ctx = PlutusV2.txSignedBy info pkh
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

{-
    As a Minting Policy
-}

compiledCode :: CompiledCode (BuiltinData -> BuiltinData -> ())
compiledCode = $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy expectedWitnessPolicy

policyScriptHash :: ScriptHash
policyScriptHash = scriptHash $ fromCompiledCode compiledCode

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript compiledCode

policyHash :: MintingPolicyHash
policyHash = PSU.V2.mintingPolicyHash policy

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
writeSerialisedScript = void $ writeFileTextEnvelope "check-witness-policy.plutus" Nothing serialisedScript

