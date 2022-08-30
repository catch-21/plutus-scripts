{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module TokenNamePolicy
  ( serialisedScriptV1,
    serialisedScriptV2,
    scriptSBSV1,
    scriptSBSV2,
    scriptV1,
    scriptV2,
    writeSerialisedScriptV1,
    writeSerialisedScriptV2,
    printPIRV2
  )
where

import           Cardano.Api                          (PlutusScriptV1,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..))
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Data.Maybe                           (fromJust)
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Value                         as Value
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import           PlutusTx                             (getPir)
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, (.))
import           Prettyprinter.Extras                 (pretty)

{-
   The validator script (checks redeemer token name is used for minting)
-}

{-# INLINEABLE tokenNamePolicyV1 #-}
tokenNamePolicyV1 :: TokenName -> PlutusV1.ScriptContext -> Bool
tokenNamePolicyV1 tn ctx = traceIfFalse "wrong token name" checkTokenName
    where
    info :: PlutusV1.TxInfo
    info = PlutusV1.scriptContextTxInfo ctx

    checkTokenName :: Bool
    checkTokenName = valueOf (PlutusV1.txInfoMint info) (PlutusV1.ownCurrencySymbol ctx) tn > 0

{-# INLINEABLE tokenNamePolicyV2 #-}
tokenNamePolicyV2 :: TokenName -> PlutusV2.ScriptContext -> Bool
tokenNamePolicyV2 tn ctx = traceIfFalse "wrong token name" checkTokenName
    where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    checkTokenName :: Bool
    checkTokenName = valueOf (PlutusV2.txInfoMint info) (PlutusV2.ownCurrencySymbol ctx) tn > 0

{-
    As a Minting Policy
-}

policyV1 :: Scripts.MintingPolicy
policyV1 = PlutusV1.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V1.mkUntypedMintingPolicy tokenNamePolicyV1 ||])

policyV2 :: Scripts.MintingPolicy
policyV2 = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| PSU.V2.mkUntypedMintingPolicy tokenNamePolicyV2 ||])

printPIRV2 = pretty $ fromJust $ getPir $$(PlutusTx.compile [|| PSU.V2.mkUntypedMintingPolicy tokenNamePolicyV2 ||])

{-
    As a Script
-}

scriptV1 :: PlutusV1.Script
scriptV1 = PlutusV1.unMintingPolicyScript policyV1

scriptV2 :: PlutusV2.Script
scriptV2 = PlutusV2.unMintingPolicyScript policyV2

{-
    As a Short Byte String
-}

scriptSBSV1 :: SBS.ShortByteString
scriptSBSV1 = SBS.toShort . LBS.toStrict $ serialise scriptV1

scriptSBSV2 :: SBS.ShortByteString
scriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise scriptV2

{-
    As a Serialised Script
-}

serialisedScriptV1 :: PlutusScript PlutusScriptV1
serialisedScriptV1 = PlutusScriptSerialised scriptSBSV1

writeSerialisedScriptV1 :: IO ()
writeSerialisedScriptV1 = void $ writeFileTextEnvelope "token-name-policy-V1.plutus" Nothing serialisedScriptV1

serialisedScriptV2 :: PlutusScript PlutusScriptV2
serialisedScriptV2 = PlutusScriptSerialised scriptSBSV2

writeSerialisedScriptV2 :: IO ()
writeSerialisedScriptV2 = void $ writeFileTextEnvelope "token-name-policy-V2.plutus" Nothing serialisedScriptV2
