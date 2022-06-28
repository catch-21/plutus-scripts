{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module UntypedHelloWorld
  ( helloWorldSerialised,
    helloWorldSBS,
    writeHelloWorldScript,
    writeHelloWorldScriptV2,
  )
where

import           Cardano.Api              (writeFileTextEnvelope)
import           Cardano.Api.Shelley      (PlutusScript (PlutusScriptSerialised),
                                           PlutusScriptV1, PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import           Data.Functor             (void)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import qualified PlutusTx.Builtins        as BI
import           PlutusTx.Prelude         as P hiding (Semigroup (..), unless,
                                                (.))
import           Prelude                  (IO, (.))

hello :: BuiltinData
hello = BI.mkB "Hello World!"

{-
   The Hello World validator script
-}

{-# INLINEABLE helloWorld #-}
helloWorld :: BuiltinData -> BuiltinData -> BuiltinData -> ()
helloWorld datum _redeemer _context = if datum P.== hello then () else (P.error ())

{-
    As a Validator
-}

helloWorldValidator :: Plutus.Validator
helloWorldValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [||helloWorld||])

{-
    As a Script
-}

helloWorldScript :: Plutus.Script
helloWorldScript = Plutus.unValidatorScript helloWorldValidator

{-
    As a Short Byte String
-}

helloWorldSBS :: SBS.ShortByteString
helloWorldSBS = SBS.toShort . LBS.toStrict $ serialise helloWorldScript

{-
    As a Serialised Script
-}

helloWorldSerialised :: PlutusScript PlutusScriptV1
helloWorldSerialised = PlutusScriptSerialised helloWorldSBS

writeHelloWorldScript :: IO ()
writeHelloWorldScript = void $ writeFileTextEnvelope "untyped-helloWorld.plutus" Nothing helloWorldSerialised

helloWorldSerialisedV2 :: PlutusScript PlutusScriptV2
helloWorldSerialisedV2 = PlutusScriptSerialised helloWorldSBS

writeHelloWorldScriptV2 :: IO ()
writeHelloWorldScriptV2 = void $ writeFileTextEnvelope "untyped-helloWorld-v2.plutus" Nothing helloWorldSerialisedV2
