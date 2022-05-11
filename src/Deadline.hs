{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Deadline
  ( serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
    runTrace,
  )
where

import           Cardano.Api                      (writeFileTextEnvelope)
import           Cardano.Api.Shelley              (PlutusScript (..),
                                                   PlutusScriptV1)
import           Codec.Serialise
import           Control.Monad.Freer.Extras       as Extras
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Short            as SBS
import           Data.Functor                     (void)
import           Data.Map                         as Map
import           Data.Text                        (Text)
import           Data.Void                        (Void)
import           Ledger
import           Ledger.Ada                       as Ada
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.TxConstraints as TxConstraints
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Typed.Scripts.Validators
import           Plutus.Contract                  as Contract
import           Plutus.Trace.Emulator            as Emulator
import qualified Plutus.V1.Ledger.Api             as Ledger.Api
import qualified Plutus.V1.Ledger.Scripts         as Plutus
import qualified PlutusTx
import qualified PlutusTx.Builtins                as BI
import           PlutusTx.Prelude                 as P hiding (Semigroup (..),
                                                        unless, (.))
import           Prelude                          (IO, Semigroup (..),
                                                   Show (..), String, putStrLn,
                                                   (.))
import           Wallet.Emulator.Wallet

{-
   The timelocked validator script
-}

deadline :: POSIXTime
deadline = 1596059095000 -- (milliseconds) transaction's valid range must be before this

{-# INLINEABLE mkValidator #-}
mkValidator :: POSIXTime -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator dl _ _ ctx = (to dl) `contains` range
    where
    --traceIfFalse (decodeUtf8 $ BI.unsafeDataAsB $ PlutusTx.toBuiltinData range) ((to dl) `contains` range)

    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

{-
    As a validator
-}

validator :: POSIXTime -> Plutus.Validator
validator t =
    Ledger.mkValidatorScript $
    $$(PlutusTx.compile [||validatorParam||])
        `PlutusTx.applyCode` PlutusTx.liftCode deadline
    where
    validatorParam s = Scripts.wrapValidator (mkValidator s)

{-
    As a Script
-}

script :: Plutus.Script
script = Plutus.unValidatorScript $ validator deadline

{-
   As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

{-
   As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV1
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "deadline.plutus" Nothing serialisedScript

{-
    Offchain Contract
-}

scrAddress :: Ledger.Address
scrAddress = scriptAddress $ validator deadline

valHash :: ValidatorHash
valHash = Ledger.validatorHash $ validator deadline

contract :: Contract () Empty Text ()
contract = do
    now <- currentTime
    Contract.logInfo @String $ "now: " ++ show now
    Contract.logInfo @String $ "1: pay the script address"
    let tx1 = Constraints.mustPayToOtherScript valHash unitDatum $ Ada.lovelaceValueOf 2000000
    ledgerTx1 <- submitTx tx1
    awaitTxConfirmed $ getCardanoTxId ledgerTx1
    Contract.logInfo @String $ "tx1 successfully submitted"
    Contract.logInfo @String $ "2: spend from script address with expected validity interval"
    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups =
            Constraints.otherScript (validator deadline)
            <> Constraints.unspentOutputs utxos
        tx2 =
            mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
            <> Constraints.mustIncludeDatum unitDatum
            <> Constraints.mustValidateIn (to $ deadline - 1001) -- cannot be 1ms before
    ledgerTx2 <- submitTxConstraintsWith @Void lookups tx2
    Contract.logInfo @String $ "waiting for tx2 confirmed..."
    awaitTxConfirmed $ getCardanoTxId ledgerTx2
    Contract.logInfo @String $ "tx2 successfully submitted"

{-
    Trace
-}

runTrace :: IO ()
runTrace = runEmulatorTraceIO emulatorTrace

emulatorTrace :: EmulatorTrace ()
emulatorTrace = do
    void $ activateContractWallet (knownWallet 1) contract
    void $ Emulator.waitNSlots 2
