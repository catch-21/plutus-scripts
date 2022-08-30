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
  )
where

import           Cardano.Api                          (PlutusScript (..),
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (PlutusScriptSerialised))
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Ledger
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import           PlutusTx                             (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, (.))

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

