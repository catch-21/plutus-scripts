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
import qualified Ledger.Typed.Scripts             as Ledger
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

deadline :: POSIXTime
deadline = 1596059095000 -- (milliseconds) transaction's valid range must be before this

{-# INLINEABLE mkValidator #-}
mkValidator :: POSIXTime -> () -> () -> ScriptContext -> Bool
mkValidator dl _ _ ctx = traceError (decodeUtf8 (disp range "")) -- to dl `contains` range
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

{-
    As a validator
-}

instance Scripts.ValidatorTypes POSIXTime where
    type instance RedeemerType POSIXTime = ()
    type instance DatumType POSIXTime = ()

typedValidator :: POSIXTime -> Scripts.TypedValidator POSIXTime
typedValidator = Scripts.mkTypedValidatorParam @POSIXTime
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

{-
    As a Script
-}

script :: POSIXTime -> Validator
script = Scripts.validatorScript . typedValidator

{-
   As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise $ script deadline

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
scrAddress = Scripts.validatorAddress $ typedValidator deadline
--scrAddress = Ledger.scriptHashAddress valHash

valHash :: ValidatorHash
valHash = validatorHash $ typedValidator deadline

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
            Constraints.otherScript (script deadline)
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
