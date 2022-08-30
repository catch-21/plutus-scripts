{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module TypedDatumRedeemer42
  ( datumRedeemer42Serialised,
    datumRedeemer42SBS,
    writeDatumRedeemer42Script,
    traceDatumRedeemer42,
  )
where

import           Cardano.Api                          (writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (PlutusScriptSerialised),
                                                       PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Data.Map                             as Map
import           Data.Text                            (Text)
import           Data.Void                            (Void)
import           Ledger
import           Ledger.Ada                           as Ada
import           Ledger.Constraints                   as Constraints
import           Ledger.Constraints.TxConstraints     as TxConstraints
import qualified Ledger.Typed.Scripts                 as Scripts
import           Ledger.Typed.Scripts.Validators
import           Plutus.Contract                      as Contract
import qualified Plutus.Script.Utils.V1.Scripts       as PSU.V1
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import           Plutus.Trace.Emulator                as Emulator
import qualified Plutus.V1.Ledger.Scripts             as Plutus
import qualified PlutusTx
import qualified PlutusTx.Builtins                    as BI
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       String, (.))
import           Wallet.Emulator.Wallet

{-
   The Typed 42 validator script
-}

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> Integer -> ScriptContext -> Bool
mkValidator d r _ =
    traceIfFalse "datum is not 42" (d == 42) &&
    traceIfFalse "redeemer is not 42" (r == 42)

{-
    As a typed validator
-}

data Typed
instance Scripts.ValidatorTypes Typed where
  type DatumType Typed = Integer
  type RedeemerType Typed = Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator =
    Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V1.mkUntypedValidator

datumRedeemer42Validator :: Plutus.Validator
datumRedeemer42Validator = validatorScript typedValidator

{-
    As a Script
-}

datumRedeemer42Script :: Plutus.Script
datumRedeemer42Script = Plutus.unValidatorScript datumRedeemer42Validator

{-
   As a Short Byte String
-}

datumRedeemer42SBS :: SBS.ShortByteString
datumRedeemer42SBS = SBS.toShort . LBS.toStrict $ serialise datumRedeemer42Script

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

scrAddress :: Ledger.Address
scrAddress = Scripts.validatorAddress typedValidator
--scrAddress = Ledger.scriptHashAddress valHash

valHash :: PSU.V1.ValidatorHash
valHash = Scripts.validatorHash typedValidator

datumRedeemer42Contract :: Contract () Empty Text ()
datumRedeemer42Contract = do
    logInfo @String $ "1: pay the script address"
    let tx1 = Constraints.mustPayToOtherScript valHash (Plutus.Datum $ BI.mkI 42) $ Ada.lovelaceValueOf 2000000
    ledgerTx1 <- submitTx tx1
    awaitTxConfirmed $ getCardanoTxId ledgerTx1
    logInfo @String $ "tx1 successfully submitted"

    logInfo @String $ "2: spend from script address including datum and redeemer 'Integer 42'"
    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups =
            Constraints.plutusV1OtherScript datumRedeemer42Validator
            <> Constraints.unspentOutputs utxos
        tx2 =
            mconcat [Constraints.mustSpendScriptOutput oref (Plutus.Redeemer $ BI.mkI 42) | oref <- orefs]
            <> Constraints.mustIncludeDatum (Plutus.Datum $ BI.mkB "Not 42") -- List comprehension -- Changing redeemer value correctly throws ValidationError
            <> Constraints.mustValidateIn (to $ 1596059100000) -- doesn't seem to care what datum is
    Contract.logDebug $ requiredDatums tx2 -- does include wrong BS datum, idk
    ledgerTx2 <- submitTxConstraintsWith @Void lookups tx2
    logInfo @String $ "waiting for tx2 confirmed..."
    awaitTxConfirmed $ getCardanoTxId ledgerTx2
    logInfo @String $ "tx2 successfully submitted"

{-
    Trace
-}

traceDatumRedeemer42 :: IO ()
traceDatumRedeemer42 = runEmulatorTraceIO datumRedeemer42Trace

datumRedeemer42Trace :: EmulatorTrace ()
datumRedeemer42Trace = do
    void $ activateContractWallet (knownWallet 1) datumRedeemer42Contract
    void $ Emulator.waitNSlots 2
