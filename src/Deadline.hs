{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wall #-}

module Deadline
  ( serialisedScriptV1,
    scriptSBSV1,
    scriptV1,
    writeSerialisedScriptV1,
    serialisedScriptV2,
    scriptSBSV2,
    scriptV2,
    writeSerialisedScriptV2,
    runTrace,
  )
where

import           Cardano.Api                          (PlutusScriptV2,
                                                       writeFileTextEnvelope)
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
import qualified Ledger.Typed.Scripts                 as Scripts
import           Plutus.Contract                      as Contract
import qualified Plutus.Script.Utils.V1.Scripts       as PSU.V1
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import           Plutus.Trace.Emulator                as Emulator (EmulatorTrace,
                                                                   activateContractWallet,
                                                                   runEmulatorTraceIO,
                                                                   waitNSlots)
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                           (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (IO, Semigroup (..),
                                                       Show (..), String, (.))
import           Wallet.Emulator.Wallet



-------------------------------------------------------------------------------
-- Disp like Show (Good for debuging time interval emulator issue, can remove once resolved)
-------------------------------------------------------------------------------

class Disp a where
    disp :: a -> BuiltinByteString -> BuiltinByteString

instance Disp a => Disp (Interval a) where
    disp (Interval lb ub) end =
        "Interval(" `appendByteString` disp lb (44 `consByteString` disp ub (41 `consByteString` end))

-- not showing the [ ( difference
instance Disp a => Disp (LowerBound a) where
    disp (LowerBound x _) end = disp x end

instance Disp a => Disp (UpperBound a) where
    disp (UpperBound x _) end = disp x end

instance Disp a => Disp (Extended a) where
    disp (Finite x) end = disp x end
    disp NegInf     end = "NegInf" `appendByteString` end
    disp PosInf     end = "PosInf" `appendByteString` end

instance Disp POSIXTime where
    disp (POSIXTime i) = disp i

instance Disp Integer where
    disp n end
        | n < 0     = 45 `consByteString` go (negate n) end
        | n == 0    = 48 `consByteString` emptyByteString
        | otherwise = go n end
      where
        go :: Integer -> BuiltinByteString -> BuiltinByteString
        go m acc
            | m == 0    = acc
            | otherwise =
                  let
                    m' = m `P.divide` 10
                    r  = m `modulo` 10
                  in
                    go m' $ consByteString (r + 48) acc




{-
   The timelocked validator script
-}

deadline :: PlutusV1.POSIXTime
deadline = 1596059095000 -- (milliseconds) transaction's valid range must be before this

{-# INLINEABLE mkValidatorV1 #-}
mkValidatorV1 :: PlutusV1.POSIXTime -> () -> () -> PlutusV1.ScriptContext -> Bool
mkValidatorV1 dl _ _ ctx =  to dl `contains` range -- traceError (decodeUtf8 (disp range ""))
    where
    info :: PlutusV1.TxInfo
    info = scriptContextTxInfo ctx

    range :: PlutusV1.POSIXTimeRange
    range = txInfoValidRange info

{-# INLINEABLE mkValidatorV2 #-}
mkValidatorV2 :: PlutusV2.POSIXTime -> () -> () -> PlutusV2.ScriptContext -> Bool
mkValidatorV2 dl _ _ ctx =  to dl `contains` range -- traceError (decodeUtf8 (disp range ""))
    where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    range :: PlutusV2.POSIXTimeRange
    range = PlutusV2.txInfoValidRange info

{-
    As a validator
-}

data DeadlineValidator
instance Scripts.ValidatorTypes DeadlineValidator

typedValidatorV1 :: PlutusV1.POSIXTime -> Scripts.TypedValidator DeadlineValidator
typedValidatorV1 = Scripts.mkTypedValidatorParam @DeadlineValidator
    $$(PlutusTx.compile [||mkValidatorV1||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PSU.V1.mkUntypedValidator

untypedValidatorV2 :: PlutusV2.POSIXTime -> PSU.V2.Validator -- There is not yet a way to make a V2 typed validator (PLT-494)
untypedValidatorV2 t = PlutusV2.mkValidatorScript $
    $$(PlutusTx.compile [|| PSU.V2.mkUntypedValidator . mkValidatorV2 ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode t

{-
    As a Script
-}

scriptV1 :: POSIXTime -> Validator
scriptV1 = Scripts.validatorScript . typedValidatorV1

scriptV2 :: POSIXTime -> PSU.V2.Validator
scriptV2 = untypedValidatorV2

{-
   As a Short Byte String
-}

scriptSBSV1 :: SBS.ShortByteString
scriptSBSV1 = SBS.toShort . LBS.toStrict $ serialise $ scriptV1 deadline

scriptSBSV2 :: SBS.ShortByteString
scriptSBSV2 = SBS.toShort . LBS.toStrict $ serialise $ scriptV2 deadline

{-
   As a Serialised Script
-}

serialisedScriptV1 :: PlutusScript PlutusScriptV1
serialisedScriptV1 = PlutusScriptSerialised scriptSBSV1

serialisedScriptV2 :: PlutusScript PlutusScriptV2
serialisedScriptV2 = PlutusScriptSerialised scriptSBSV2

writeSerialisedScriptV1 :: IO ()
writeSerialisedScriptV1 = void $ writeFileTextEnvelope "deadline-V1.plutus" Nothing serialisedScriptV1

writeSerialisedScriptV2 :: IO ()
writeSerialisedScriptV2 = void $ writeFileTextEnvelope "deadline-V2.plutus" Nothing serialisedScriptV2

{-
    Offchain Contract
-}

scrAddress :: Ledger.Address
scrAddress = Scripts.validatorAddress $ typedValidatorV1 deadline
--scrAddress = Ledger.scriptHashAddress valHash

valHash :: PSU.V1.ValidatorHash
valHash = Scripts.validatorHash $ typedValidatorV1 deadline

contract :: Contract () Empty Text ()
contract = do
    now <- currentTime
    Contract.logInfo @String $ "now: " ++ show now
    Contract.logInfo @String $ "1: pay the script address"
    let tx1 = Constraints.mustPayToOtherScript valHash unitDatum $ Ada.lovelaceValueOf 25000000
    ledgerTx1 <- submitTx tx1
    awaitTxConfirmed $ getCardanoTxId ledgerTx1
    Contract.logInfo @String $ "tx1 successfully submitted"
    Contract.logInfo @String $ "2: spend from script address with expected validity interval"
    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups =
            Constraints.plutusV1OtherScript (scriptV1 deadline)
            <> Constraints.unspentOutputs utxos
        tx2 =
            mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
            <> Constraints.mustIncludeDatum unitDatum
            <> Constraints.mustValidateIn (from $ now - 1000) -- FAILS
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
