{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypedDatumRedeemer42
  ( datumRedeemer42Serialised
  , datumRedeemer42SBS
  , writeDatumRedeemer42Script
--  , traceDatumRedeemer42
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
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Typed.Scripts.Validators

import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import qualified Plutus.V1.Ledger.Scripts   as Plutus
import qualified Plutus.V1.Ledger.Api       as Ledger.Api
import qualified PlutusTx
import qualified PlutusTx.Builtins          as BI
import           PlutusTx.Prelude           as P hiding (Semigroup (..), (.), unless)

import           Wallet.Emulator.Wallet

{-
   The Typed 42 validator script
-}

{-# INLINABLE mkValidator #-}
mkValidator :: Integer -> Integer -> ScriptContext -> Bool
mkValidator d r _ =
    traceIfFalse "datum is not 42"    (d == 42) &&
    traceIfFalse "redeemer is not 42" (r == 42)

{-
    As a typed validator
-}

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = Integer
    type instance RedeemerType Typed = Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @Integer


datumRedeemer42Validator :: Plutus.Validator
datumRedeemer42Validator = Scripts.validatorScript typedValidator

--valHash :: Ledger.ValidatorHash
--valHash = Scripts.validatorHash typedValidator

--scrAddress :: Ledger.Address
--scrAddress = scriptAddress validator

{-
    As a Script
-}

datumRedeemer42Script :: Plutus.Script
datumRedeemer42Script = Plutus.unValidatorScript datumRedeemer42Validator

{-
   As a Short Byte String
-}

datumRedeemer42SBS :: SBS.ShortByteString
datumRedeemer42SBS =  SBS.toShort . LBS.toStrict $ serialise datumRedeemer42Script

{-
   As a Serialised Script
-}

datumRedeemer42Serialised :: PlutusScript PlutusScriptV1
datumRedeemer42Serialised = PlutusScriptSerialised datumRedeemer42SBS

writeDatumRedeemer42Script :: IO ()
writeDatumRedeemer42Script = void $ writeFileTextEnvelope "typed-datum-redeemer-42.plutus" Nothing datumRedeemer42Serialised

{-
    Offchain Contract
-}
{-
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

-}
