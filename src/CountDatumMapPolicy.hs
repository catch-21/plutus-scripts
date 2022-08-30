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

module CountDatumMapPolicy
  ( serialisedScript,
    scriptSBS,
    script,
    writeSerialisedScript,
  )
where

import           Cardano.Api                          (PlutusScript,
                                                       PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (..))
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P (Bool, Eq ((==)),
                                                            Integer,
                                                            Maybe (Nothing),
                                                            length,
                                                            traceIfFalse, ($))
import           Prelude                              (IO, (.))

{-
   The validator script
-}

{-# INLINEABLE countDatumsPolicy #-}
countDatumsPolicy :: Integer -> PlutusV2.ScriptContext -> Bool
countDatumsPolicy n ctx =  traceIfFalse "Number of datums in txInfoData map does not match expected" $ n P.== P.length (PlutusV2.txInfoData info)
    where
        info :: PlutusV2.TxInfo
        info = PlutusV2.scriptContextTxInfo ctx

{-
    As a Minting Policy
-}

policy :: Scripts.MintingPolicy
policy = PlutusV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = PSU.V2.mkUntypedMintingPolicy countDatumsPolicy

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
writeSerialisedScript = void $ writeFileTextEnvelope "count-datum-map-policy.plutus" Nothing serialisedScript

