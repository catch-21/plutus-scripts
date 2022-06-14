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
    --traceHelloWorld,
  )
where

import           Cardano.Api                     (PlutusScriptVersion (PlutusScriptV2),
                                                  writeFileTextEnvelope)
import           Cardano.Api.Shelley             (PlutusScript (PlutusScriptSerialised),
                                                  PlutusScriptV1,
                                                  PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as SBS
import           Data.Functor                    (void)
import           Data.Map                        as Map
import           Data.Text                       (Text)
import           Data.Void                       (Void)
import           Ledger
import           Ledger.Ada                      as Ada
import           Ledger.Constraints              as Constraints
import qualified Ledger.Typed.Scripts            as Ledger
import           Ledger.Typed.Scripts.Validators as Scripts
import           Plutus.Contract                 as Contract
import qualified Plutus.Script.Utils.V1.Scripts  as PSU.V1
import qualified Plutus.Script.Utils.V2.Scripts  as PSU.V2
import           Plutus.Trace.Emulator           as Emulator
import qualified Plutus.V1.Ledger.Api            as Ledger.Api
import qualified Plutus.V1.Ledger.Scripts        as Plutus
import qualified PlutusTx
import qualified PlutusTx.Builtins               as BI
import           PlutusTx.Prelude                as P hiding (Semigroup (..),
                                                       unless, (.))
import           Prelude                         (IO, Semigroup (..), Show (..),
                                                  String, putStrLn, (.))
import           Wallet.Emulator.Wallet

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

{-
    Offchain Contract
-}
{-
scrAddress :: Ledger.Address
scrAddress = mkValidatorAddress helloWorldValidator
--scrAddress = Ledger.scriptHashAddress valHash

valHash :: PSU.V1.ValidatorHash
valHash = PSU.V1.validatorHash helloWorldValidator -- TODO: use Plutus.Script.Utils.V1.Scripts.validatorHash from plutus-script-utils when merged into next-node branch

helloWorldContract :: Contract () Empty Text ()
helloWorldContract = do
    logInfo @String $ "1: pay the script address"
    let tx1 = Constraints.mustPayToOtherScript valHash (Plutus.Datum $ hello) $ Ada.lovelaceValueOf 2000000
    ledgerTx1 <- submitTx tx1
    awaitTxConfirmed $ getCardanoTxId ledgerTx1

    logInfo @String $ "2: spend from script address including \"Hello World!\" datum"
    utxos <- utxosAt scrAddress
    let orefs = fst <$> Map.toList utxos
        lookups =
            Constraints.otherScript helloWorldValidator
            <> Constraints.unspentOutputs utxos
        tx2 =
            mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]
            <> Constraints.mustIncludeDatum (Plutus.Datum $ BI.mkB "Not Hello World") -- List comprehension
            -- doesn't seem to care what datum is
    ledgerTx2 <- submitTxConstraintsWith @Void lookups tx2
    awaitTxConfirmed $ getCardanoTxId ledgerTx2
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
