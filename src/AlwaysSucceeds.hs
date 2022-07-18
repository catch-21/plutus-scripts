{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module AlwaysSucceeds
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


{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

alwaysSuccedsValidator :: Plutus.Validator
alwaysSuccedsValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [||mkValidator||])


alwaysSuccedsScript :: Plutus.Script
alwaysSuccedsScript = Plutus.unValidatorScript alwaysSuccedsValidator


alwaysSuccedsSBS :: SBS.ShortByteString
alwaysSuccedsSBS = SBS.toShort . LBS.toStrict $ serialise alwaysSuccedsScript


alwaysSuccedsSerialised :: PlutusScript PlutusScriptV1
alwaysSuccedsSerialised = PlutusScriptSerialised alwaysSuccedsSBS

writeAlwaysSuccedsScript :: IO ()
writeAlwaysSuccedsScript = void $ writeFileTextEnvelope "alwayssucceeds.plutus" Nothing alwaysSuccedsSerialised

alwaysSuccedsSerialisedV2 :: PlutusScript PlutusScriptV2
alwaysSuccedsSerialisedV2 = PlutusScriptSerialised alwaysSuccedsSBS

writeAlwaysSuccedsScriptV2 :: IO ()
writeAlwaysSuccedsScriptV2 = void $ writeFileTextEnvelope "alwayssucceeds-v2.plutus" Nothing alwaysSuccedsSerialisedV2
