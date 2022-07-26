{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module SchnorrSecp256k1Validator (writeSerialisedScript) where

import           Cardano.Api           (PlutusScript, PlutusScriptV2,
                                        writeFileTextEnvelope)
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Functor          (void)
import qualified Ledger.Typed.Scripts  as Scripts
import qualified Plutus.V2.Ledger.Api  as PlutusV2
import qualified PlutusTx
import qualified PlutusTx.Builtins     as BI
import           PlutusTx.Prelude      as P hiding (Semigroup (..), unless, (.))
import           Prelude               (IO, (.))

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData ->  BuiltinData -> ()
mkPolicy red _ =
  case PlutusV2.fromBuiltinData red of
    Nothing -> P.traceError "Trace error: Invalid redeemer"
    Just (vkey, msg, sig) ->
      if BI.verifySchnorrSecp256k1Signature vkey msg sig
        then ()
        else P.traceError "Trace error: Schnorr validation failed"

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicy ||])

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript policy

scriptShortBs :: SBS.ShortByteString
scriptShortBs = SBS.toShort . LBS.toStrict $ serialise script


serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptShortBs

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "schnorr-secp256k-policy.plutus" Nothing serialisedScript

