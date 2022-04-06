{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module UntypedHelloWorld
  ( helloWorldSerialised
  , helloWorldSBS
  , writeHelloWorldSerialisedScript
  , traceHelloWorld
  ) where

import           Prelude (IO, putStrLn, Semigroup (..), (.), Show (..), String)

import           Cardano.Api (writeFileTextEnvelope)
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Short      as SBS
import qualified Data.ByteString.Lazy       as LBS
import           Data.Functor (void)
import           Data.Map                   as Map
import           Data.Text                  (Text)
import           Data.Void                  (Void)

import           Ledger
import           Ledger.Ada                 as Ada
import           Ledger.Constraints         as Constraints
import           Ledger.Typed.Scripts.Validators

import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import qualified Plutus.V1.Ledger.Scripts   as Plutus
import qualified Plutus.V1.Ledger.Api       as Ledger.Api
import qualified PlutusTx
import qualified PlutusTx.Builtins          as BI
import           PlutusTx.Prelude           as P hiding (Semigroup (..), (.), unless)

import           Wallet.Emulator.Wallet

hello :: BuiltinData
hello = BI.mkB "Hello World!"

{-
   The Hello World validator script
-}

{-# INLINABLE helloWorld #-}

helloWorld :: BuiltinData -> BuiltinData -> BuiltinData -> ()
helloWorld datum _redeemer _context = if datum P.== hello then () else (P.error ())

{-
    As a Validator
-}

helloWorldValidator :: Plutus.Validator
helloWorldValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| helloWorld ||])

{-
    As a Script
-}

helloWorldScript :: Plutus.Script
helloWorldScript = Plutus.unValidatorScript helloWorldValidator

{-
    As a Short Byte String
-}

helloWorldSBS :: SBS.ShortByteString
helloWorldSBS =  SBS.toShort . LBS.toStrict $ serialise helloWorldScript

{-
    As a Serialised Script
-}

helloWorldSerialised :: PlutusScript PlutusScriptV1
helloWorldSerialised = PlutusScriptSerialised helloWorldSBS

writeHelloWorldSerialisedScript :: IO ()
writeHelloWorldSerialisedScript = void $ writeFileTextEnvelope "untyped-helloWorld.plutus" Nothing helloWorldSerialised  

{-
    Offchain Contract
-}

scrAddress :: Ledger.Address
scrAddress = scriptAddress helloWorldValidator

valHash :: ValidatorHash
valHash = Ledger.validatorHash helloWorldValidator

helloWorldContract :: Contract () Empty Text ()
helloWorldContract = do
    logInfo @String $ "1: pay the script address"
    let tx1 = Constraints.mustPayToOtherScript valHash (Plutus.Datum $ hello) $ Ada.lovelaceValueOf 2000000
    ledgerTx <- submitTx tx1
    awaitTxConfirmed $ getCardanoTxId ledgerTx
 
    logInfo @String $ "2: spend from script address including \"Hello World!\" datum"
    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups = Constraints.otherScript helloWorldValidator <>
                  Constraints.unspentOutputs utxos
        tx2 = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <> -- List comprehension
              Constraints.mustIncludeDatum (Plutus.Datum $ BI.mkB "Not Hello World") -- doesn't seem to care what datum is
    ledgerTx <- submitTxConstraintsWith @Void lookups tx2
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "\"Hello World!\" tx successfully submitted"

{-
    Trace 
-}

traceHelloWorld :: IO ()
traceHelloWorld = runEmulatorTraceIO helloWorldTrace

helloWorldTrace :: EmulatorTrace ()
helloWorldTrace = do
    void $ activateContractWallet (knownWallet 1) helloWorldContract
    void $ Emulator.nextSlot
