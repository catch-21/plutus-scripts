{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module CheckDatumPolicy
  ( printScriptData,
    myDatum,
    redeemerDatum,
    redeemerDatumHash,
    redeemerNoOutputDatum,
    serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
    --  , runTrace
  )
where

import           Cardano.Api                         (writeFileTextEnvelope)
import           Cardano.Api.Shelley                 (PlutusScript (..),
                                                      PlutusScriptV2,
                                                      ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                      fromPlutusData,
                                                      scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                          as A
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import           Data.Functor                        (void)
import qualified Data.Text.Internal.ByteStringCompat as BI
import           Ledger.Ada                          as Ada
import           Ledger.Constraints                  as Constraints
import qualified Ledger.Typed.Scripts                as Scripts
import           Ledger.Typed.Scripts.Validators
import           Ledger.Value                        as Value
import           Plutus.Contract                     as Contract
import           Plutus.Contract.Schema              (Input)
import qualified Plutus.Script.Utils.V2.Scripts      as PSU.V2
import           Plutus.Trace.Emulator               as Emulator
import qualified Plutus.V2.Ledger.Api                as PlutusV2
import qualified Plutus.V2.Ledger.Contexts           as PlutusV2
import           Plutus.V2.Ledger.Tx
import qualified PlutusTx
import qualified PlutusTx.Builtins                   as BI
import           PlutusTx.Prelude                    as P hiding
                                                          (Semigroup (..),
                                                           unless, (.))
import           Prelude                             (IO, Semigroup (..),
                                                      Show (..), String, print,
                                                      putStrLn, (.))
import           Wallet.Emulator.Wallet

{-
   Define redeemer type to handle expected inline datum or datum hash at a txo
-}

data InputType = RegularInput | ReferenceInput | BothInputTypes
    deriving (Show)

PlutusTx.unstableMakeIsData ''InputType

data ExpInputDatum = ExpInputDatum
        { txOutRef  :: PlutusV2.TxOutRef,
          expDatum  :: PlutusV2.OutputDatum,
          inputType :: InputType
        }
    deriving (Show)

PlutusTx.unstableMakeIsData ''ExpInputDatum

{-
   Expected inline datum to use in redeemer
-}

data SomeData = SomeData {name :: BuiltinByteString, age :: Integer, shopping :: [BuiltinByteString]}

PlutusTx.unstableMakeIsData ''SomeData

someData = SomeData {name = "cats", age = 42, shopping = ["apple", "tomato", "cheese"]}

fortyTwo = 42 :: Integer

myDatum = PlutusV2.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData someData

myDatumHash = PSU.V2.datumHash myDatum

{-
   Redeemers
-}

redeemerDatum = ExpInputDatum { txOutRef  = PlutusV2.TxOutRef {txOutRefId = "b204b4554a827178b48275629e5eac9bde4f5350badecfcd108d87446f00bf26", txOutRefIdx = 0}
                              , expDatum  = PlutusV2.OutputDatum myDatum
                              , inputType = RegularInput
                              }

redeemerDatumHash = ExpInputDatum { txOutRef  = PlutusV2.TxOutRef {txOutRefId = "b204b4554a827178b48275629e5eac9bde4f5350badecfcd108d87446f00bf26", txOutRefIdx = 0}
                                  , expDatum  = PlutusV2.OutputDatumHash myDatumHash
                                  , inputType = RegularInput
                                  }

redeemerNoOutputDatum = ExpInputDatum { txOutRef  = PlutusV2.TxOutRef {txOutRefId = "b204b4554a827178b48275629e5eac9bde4f5350badecfcd108d87446f00bf26", txOutRefIdx = 0}
                                  , expDatum  = PlutusV2.NoOutputDatum
                                  , inputType = BothInputTypes
                                  }

printScriptData d = print $ "Script Data: " <> A.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ PlutusV2.toData d)

{-
   The validator script
-}

{-# INLINEABLE expectedInlinePolicy #-}
expectedInlinePolicy :: ExpInputDatum -> PlutusV2.ScriptContext -> Bool
expectedInlinePolicy expInline ctx =
    case expInline of
        ExpInputDatum _ PlutusV2.NoOutputDatum RegularInput   -> noOutputDatumInInput
        ExpInputDatum _ PlutusV2.NoOutputDatum ReferenceInput -> noOutputDatumInRefInput
        ExpInputDatum _ PlutusV2.NoOutputDatum BothInputTypes -> noOutputDatumInInput && noOutputDatumInRefInput

        ExpInputDatum _ (PlutusV2.OutputDatum d) RegularInput   -> datumInInput    d
        ExpInputDatum _ (PlutusV2.OutputDatum d) ReferenceInput -> datumInRefInput d
        ExpInputDatum _ (PlutusV2.OutputDatum d) BothInputTypes -> datumInInput    d && datumInRefInput d

        ExpInputDatum _ (PlutusV2.OutputDatumHash dh) RegularInput   -> datumHashInInput    dh
        ExpInputDatum _ (PlutusV2.OutputDatumHash dh) ReferenceInput -> datumHashInRefInput dh
        ExpInputDatum _ (PlutusV2.OutputDatumHash dh) BothInputTypes -> datumHashInInput    dh && datumHashInRefInput dh

        _ -> traceError "Unexpected case"
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

        fromJust' :: BuiltinString -> Maybe a -> a -- should be built-in
        fromJust' err Nothing = traceError err
        fromJust' _ (Just x)  = x

        findTxIn :: PlutusV2.TxInInfo
        findTxIn = fromJust' "txIn doesn't exist" $ PlutusV2.findTxInByTxOutRef (txOutRef expInline) info

        findRefTxInByTxOutRef :: TxOutRef -> PlutusV2.TxInfo -> Maybe PlutusV2.TxInInfo -- similar to findTxInByTxOutRef, should be a built-in context
        findRefTxInByTxOutRef outRef PlutusV2.TxInfo{txInfoReferenceInputs} =
            find (\PlutusV2.TxInInfo{txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoReferenceInputs

        findRefTxIn :: PlutusV2.TxInInfo
        findRefTxIn = fromJust' "txRefIn doesn't exist" $ findRefTxInByTxOutRef (txOutRef expInline) info

        noOutputDatumInInput = traceIfFalse "Expected regular input to have no output datum" $ PlutusV2.NoOutputDatum      == PlutusV2.txOutDatum (PlutusV2.txInInfoResolved findTxIn)
        datumInInput d       = traceIfFalse "Expected regular input to have datum"           $ PlutusV2.OutputDatum d      == PlutusV2.txOutDatum (PlutusV2.txInInfoResolved findTxIn)
        datumHashInInput dh  = traceIfFalse "Expected regular input to have datum hash"      $ PlutusV2.OutputDatumHash dh == PlutusV2.txOutDatum (PlutusV2.txInInfoResolved findTxIn)

        noOutputDatumInRefInput = traceIfFalse "Expected reference input to have no output datum" $ PlutusV2.NoOutputDatum      == PlutusV2.txOutDatum (PlutusV2.txInInfoResolved findRefTxIn)
        datumInRefInput d       = traceIfFalse "Expected regular input to have datum"             $ PlutusV2.OutputDatum d      == PlutusV2.txOutDatum (PlutusV2.txInInfoResolved findRefTxIn)
        datumHashInRefInput dh  = traceIfFalse "Expected regular input to have datum hash"        $ PlutusV2.OutputDatumHash dh == PlutusV2.txOutDatum (PlutusV2.txInInfoResolved findRefTxIn)

{-
    As a Minting Policy
-}

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
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
writeSerialisedScript = void $ writeFileTextEnvelope "check-datum.plutus" Nothing serialisedScript

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
