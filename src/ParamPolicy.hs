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

module ParamPolicy
  ( printRedeemer,
    serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript
  )
where

import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
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
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import qualified PlutusTx.Builtins                    as BI
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       print, (.))

{-
   Redeemers
-}

--data ExpRedeemers = ExpRedeemers {redeemers :: [Plutus.Redeemer]}

--PlutusTx.unstableMakeIsData ''ExpRedeemers

asRedeemer :: PlutusTx.ToData a => a -> PlutusV2.Redeemer
asRedeemer a = PlutusV2.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

intAsRedeemer :: Integer -> PlutusV2.Redeemer
intAsRedeemer = asRedeemer @Integer

redeemer :: [PlutusV2.Redeemer]
redeemer =  [intAsRedeemer 42, intAsRedeemer 43, asRedeemer @BI.BuiltinByteString "d"]

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)

{-
   The validator script
-}


{-# INLINEABLE paramsPolicy #-}
paramsPolicy :: BuiltinByteString -> [PlutusV2.Redeemer] -> PlutusV2.ScriptContext -> Bool
paramsPolicy bs expRedeemers ctx =  True

{-
    As a Minting Policy
-}

-- Example of how to paramaterise minting policy
policy :: BuiltinByteString -> Scripts.MintingPolicy
policy s = PlutusV2.mkMintingPolicyScript $
        $$(PlutusTx.compile [||PSU.V2.mkUntypedMintingPolicy . paramsPolicy||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode s

{-
    As a Script
-}

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript $ policy "cats"

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
writeSerialisedScript = void $ writeFileTextEnvelope "params-policy.plutus" Nothing serialisedScript

