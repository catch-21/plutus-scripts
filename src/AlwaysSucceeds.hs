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
  ( alwaysSucceedsSerialised,
    alwaysSucceedsSBS,
    writeAlwaysSucceedsScript,
    writeAlwaysSucceedsScriptV2,
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
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

alwaysSucceedsValidator :: Plutus.Validator
alwaysSucceedsValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| alwaysSucceeds ||])


alwaysSucceedsScript :: Plutus.Script
alwaysSucceedsScript = Plutus.unValidatorScript alwaysSucceedsValidator


alwaysSucceedsSBS :: SBS.ShortByteString
alwaysSucceedsSBS = SBS.toShort . LBS.toStrict $ serialise alwaysSucceedsScript


alwaysSucceedsSerialised :: PlutusScript PlutusScriptV1
alwaysSucceedsSerialised = PlutusScriptSerialised alwaysSucceedsSBS

writeAlwaysSucceedsScript :: IO ()
writeAlwaysSucceedsScript = void $ writeFileTextEnvelope "alwayssucceeds.plutus" Nothing alwaysSucceedsSerialised

alwaysSuccedsSerialisedV2 :: PlutusScript PlutusScriptV2
alwaysSuccedsSerialisedV2 = PlutusScriptSerialised alwaysSuccedsSBS

writeAlwaysSucceedsScriptV2 :: IO ()
writeAlwaysSucceedsScriptV2 = void $ writeFileTextEnvelope "alwayssucceeds-v2.plutus" Nothing alwaysSucceedsSerialisedV2
