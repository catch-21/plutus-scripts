{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module CheckRedeemersPolicy
  ( printRedeemer,
    serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
    --  , runTrace
  )
where

import           Cardano.Api                     (PlutusScript, PlutusScriptV2,
                                                  writeFileTextEnvelope)
import           Cardano.Api.Shelley             (PlutusScript (..),
                                                  ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                  fromPlutusData,
                                                  scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                      as A
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as SBS
import           Data.Functor                    (void)
--import           Ledger
import           Ledger                          (MintingPolicy (getMintingPolicy))
import           Ledger.Ada                      as Ada
import           Ledger.Constraints              as Constraints
import qualified Ledger.Typed.Scripts            as Scripts
import           Ledger.Typed.Scripts.Validators
import           Ledger.Value                    as Value
import           Plutus.Contract                 as Contract
import qualified Plutus.Script.Utils.V2.Scripts  as PSU.V2
import           Plutus.Trace.Emulator           as Emulator
import qualified Plutus.V1.Ledger.Api            as PlutusV1
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import           Plutus.V2.Ledger.Contexts       (ownCurrencySymbol)
import qualified PlutusTx
import qualified PlutusTx.AssocMap               as AMap
import qualified PlutusTx.Builtins               as BI
import           PlutusTx.Prelude                as P hiding (Semigroup (..),
                                                       unless, (.))
import           Prelude                         (IO, Semigroup (..), Show (..),
                                                  String, print, putStrLn, (.))
import           Wallet.Emulator.Wallet

{-
   Redeemers
-}

--data ExpRedeemers = ExpRedeemers {redeemers :: [Plutus.Redeemer]}

--PlutusTx.unstableMakeIsData ''ExpRedeemers

asRedeemer :: PlutusTx.ToData a => a -> PlutusV2.Redeemer
asRedeemer a = PlutusV2.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

intAsRedeemer :: Integer -> PlutusV2.Redeemer
intAsRedeemer = asRedeemer @Integer

redeemer :: [PlutusV2.Redeemer]
redeemer =  [intAsRedeemer 42, intAsRedeemer 43, asRedeemer @BI.BuiltinByteString "d"]

printRedeemer = print $ "Redeemer: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData redeemer)

{-
   The validator script
-}

{-# INLINEABLE expectedInlinePolicy #-}
expectedInlinePolicy :: [PlutusV2.Redeemer] -> PlutusV2.ScriptContext -> Bool
expectedInlinePolicy expRedeemers ctx =  traceIfFalse "Redeemers do not match expected" $ P.all ((P.== True) . findR) expRedeemers            &&
                                         traceIfFalse "Number of redeemers (without own) does not match expected" (P.length expRedeemers P.== P.length withoutOwnRedeemer)
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        thisScriptPurpose :: PlutusV2.ScriptPurpose
        thisScriptPurpose = PlutusV2.Minting $ ownCurrencySymbol ctx

        withoutOwnRedeemer :: [PlutusV2.Redeemer]
        withoutOwnRedeemer = AMap.elems $ AMap.delete thisScriptPurpose (PlutusV2.txInfoRedeemers info)

        findR :: PlutusV2.Redeemer -> Bool
        findR r = P.isJust $ P.find (P.== r) withoutOwnRedeemer
{-
    As a Minting Policy
-}

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy expectedInlinePolicy

{-
    As a Script
-}

script :: PlutusV2.Script
script = PlutusV2.unMintingPolicyScript policy

{-
    As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

{-
    As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "check-redeemers-policy.plutus" Nothing serialisedScript

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
