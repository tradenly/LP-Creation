{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module LiquidityPoolContract where

import           PlutusTx
import           PlutusTx.Prelude
import           Ledger
import           Ledger.Ada
import           Ledger.Value
import           Ledger.Typed.Scripts
import           Ledger.TimeSlot
import           Plutus.Contract
import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Contexts
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Interval
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Prelude (IO, Show)
import qualified Prelude

-- Define the liquidity pool parameters
data LiquidityPoolParams = LiquidityPoolParams
    { assetA         :: AssetClass  -- First asset in the pool
    , assetB         :: AssetClass  -- Second asset in the pool
    , poolFee        :: Rational    -- Pool fee (e.g., 0.3% = 3/1000)
    , creator        :: PubKeyHash  -- Pool creator
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the liquidity pool action
data LiquidityPoolAction
    = CreatePool
    | AddLiquidity
    | RemoveLiquidity
    | ClaimRewards
    deriving (Show, Generic, ToJSON, FromJSON)

-- Define the liquidity pool datum
data LiquidityPoolDatum = LiquidityPoolDatum
    { poolId         :: Integer     -- Unique pool ID
    , totalLiquidity :: Integer     -- Total liquidity in the pool
    , lpTokens       :: Integer     -- Total LP tokens issued
    , rewards        :: Integer     -- Total rewards (BOTLY tokens)
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the liquidity pool redeemer
data LiquidityPoolRedeemer = LiquidityPoolRedeemer
    { action         :: LiquidityPoolAction
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the liquidity pool validator
liquidityPoolValidator :: LiquidityPoolParams -> LiquidityPoolDatum -> LiquidityPoolRedeemer -> ScriptContext -> Bool
liquidityPoolValidator params datum redeemer ctx =
    case action redeemer of
        CreatePool ->
            -- Validate pool creation
            traceIfFalse "Invalid pool creation" (validatePoolCreation params datum ctx)
        AddLiquidity ->
            -- Validate liquidity addition
            traceIfFalse "Invalid liquidity addition" (validateAddLiquidity params datum ctx)
        RemoveLiquidity ->
            -- Validate liquidity removal
            traceIfFalse "Invalid liquidity removal" (validateRemoveLiquidity params datum ctx)
        ClaimRewards ->
            -- Validate reward claiming
            traceIfFalse "Invalid reward claim" (validateClaimRewards params datum ctx)
  where
    info = scriptContextTxInfo ctx

-- Helper function to validate pool creation
validatePoolCreation :: LiquidityPoolParams -> LiquidityPoolDatum -> ScriptContext -> Bool
validatePoolCreation params datum ctx =
    let fee = poolFee params
    in fee >= 5 % 10000 && fee <= 20 % 100  -- Fee must be between 0.05% and 20%
    && traceIfFalse "Invalid fee" (fee >= 5 % 10000 && fee <= 20 % 100)

-- Helper function to validate liquidity addition
validateAddLiquidity :: LiquidityPoolParams -> LiquidityPoolDatum -> ScriptContext -> Bool
validateAddLiquidity params datum ctx =
    let liquidityAdded = getLiquidityAdded info (assetA params) (assetB params)
        lpTokensIssued = calculateLPTokens liquidityAdded (totalLiquidity datum) (lpTokens datum)
    in lpTokensIssued > 0
    && traceIfFalse "Invalid liquidity addition" (lpTokensIssued > 0)

-- Helper function to validate liquidity removal
validateRemoveLiquidity :: LiquidityPoolParams -> LiquidityPoolDatum -> ScriptContext -> Bool
validateRemoveLiquidity params datum ctx =
    let liquidityRemoved = getLiquidityRemoved info (assetA params) (assetB params)
        lpTokensBurned = calculateLPTokens liquidityRemoved (totalLiquidity datum) (lpTokens datum)
    in lpTokensBurned > 0
    && traceIfFalse "Invalid liquidity removal" (lpTokensBurned > 0)

-- Helper function to validate reward claiming
validateClaimRewards :: LiquidityPoolParams -> LiquidityPoolDatum -> ScriptContext -> Bool
validateClaimRewards params datum ctx =
    let rewardsClaimed = getRewardsClaimed info
    in rewardsClaimed <= rewards datum
    && traceIfFalse "Invalid reward claim" (rewardsClaimed <= rewards datum)

-- Helper function to calculate LP tokens
calculateLPTokens :: Integer -> Integer -> Integer -> Integer
calculateLPTokens liquidityAdded totalLiquidity lpTokens =
    if totalLiquidity == 0
        then liquidityAdded
        else liquidityAdded * lpTokens `divide` totalLiquidity

-- Compile the validator
liquidityPoolValidatorCompiled :: LiquidityPoolParams -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
liquidityPoolValidatorCompiled params = $$(compile [|| \d r ctx -> liquidityPoolValidator params (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData ctx) ||])

-- Define the liquidity pool script
liquidityPoolScript :: LiquidityPoolParams -> Script
liquidityPoolScript params = mkValidatorScript (liquidityPoolValidatorCompiled params)

-- Define the liquidity pool address
liquidityPoolAddress :: LiquidityPoolParams -> Address
liquidityPoolAddress params = scriptHashAddress (validatorHash (liquidityPoolScript params))

-- Define the liquidity pool contract
liquidityPoolContract :: LiquidityPoolParams -> Contract () LiquidityPoolSchema Text ()
liquidityPoolContract params = do
    -- Create pool action
    handleCreatePool <- endpoint @"createPool" $ \(creator, assetA, assetB, fee) -> do
        let datum = LiquidityPoolDatum { poolId = 1, totalLiquidity = 0, lpTokens = 0, rewards = 0 }
        let tx = mustPayToTheScript datum (assetClassValue (assetClass "" "ADA") 0)
        submitTxConstraints (liquidityPoolScript params) tx
        awaitTxConfirmed (getCardanoTxId tx)
        logInfo @Text $ "Pool created for assets " <> show assetA <> " and " <> show assetB

    -- Add liquidity action
    handleAddLiquidity <- endpoint @"addLiquidity" $ \(poolId, amountA, amountB) -> do
        let datum = LiquidityPoolDatum { poolId = poolId, totalLiquidity = 0, lpTokens = 0, rewards = 0 }
        let tx = mustPayToTheScript datum (assetClassValue (assetA params) amountA <> assetClassValue (assetB params) amountB)
        submitTxConstraints (liquidityPoolScript params) tx
        awaitTxConfirmed (getCardanoTxId tx)
        logInfo @Text $ "Liquidity added to pool " <> show poolId

    -- Combine the handlers
    selectList [handleCreatePool, handleAddLiquidity]

-- Define the schema
type LiquidityPoolSchema =
    Endpoint "createPool" (PubKeyHash, AssetClass, AssetClass, Rational)
    .\/ Endpoint "addLiquidity" (Integer, Integer, Integer)

-- Define the main function
main :: IO ()
main = do
    -- Define the liquidity pool parameters
    let params = LiquidityPoolParams
            { assetA = assetClass "BOTLY" "BOTLY"
            , assetB = assetClass "" "ADA"
            , poolFee = 3 % 1000  -- 0.3% fee
            , creator = "pubKeyHash1"
            }

    -- Run the liquidity pool contract
    runPlutusApp $ liquidityPoolContract params
