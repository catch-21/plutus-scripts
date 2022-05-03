{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TokenNamePolicy
  ( serialisedScript
  , scriptSBS
  , script
  , writeSerialisedScript
--  , runTrace
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
import           Ledger.Value               as Value

import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import qualified Plutus.V1.Ledger.Scripts   as Plutus
import qualified Plutus.V1.Ledger.Api       as Ledger.Api
import qualified PlutusTx
import qualified PlutusTx.Builtins          as BI
import           PlutusTx.Prelude           as P hiding (Semigroup (..), (.), unless)

import           Wallet.Emulator.Wallet

{-
   The validator script (checks redeemer token name is used for minting)
-}

{-# INLINABLE tokenNamePolicy #-}

tokenNamePolicy :: TokenName -> ScriptContext -> Bool
tokenNamePolicy tn ctx = traceIfFalse "wrong token name" checkTokenName

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkTokenName :: Bool
    checkTokenName = valueOf (txInfoMint info) (ownCurrencySymbol ctx) tn > 0
{-
    As a Minting Policy
-}

policy :: Scripts.MintingPolicy
policy = Plutus.mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy tokenNamePolicy ||])

{-
    As a Script
-}

script :: Plutus.Script
script = Plutus.unMintingPolicyScript policy

{-
    As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS =  SBS.toShort . LBS.toStrict $ serialise script

{-
    As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV1
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "mint-tokenname.plutus" Nothing serialisedScript

{-

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

-}
