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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Homework1 where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO)
import qualified Prelude              as P
import           Text.Printf          (printf)

data VestingDatum = VestingDatum
    { beneficiary1 :: PaymentPubKeyHash
    , beneficiary2 :: PaymentPubKeyHash
    , deadline     :: POSIXTime
    } deriving P.Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx 
    | isBeforeOrEqualDeadline && signedByBeneficiary1 = True
    | isAfterDeadline && signedByBeneficiary2 = True
    | otherwise = traceError "invalid conditions wey" 
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        isBeforeOrEqualDeadline :: Bool
        isBeforeOrEqualDeadline = contains (to $ deadline dat) $ txInfoValidRange info

        signedByBeneficiary1 :: Bool
        signedByBeneficiary1 = txSignedBy info (unPaymentPubKeyHash $ beneficiary1 dat)
 
        signedByBeneficiary2 :: Bool
        signedByBeneficiary2 = txSignedBy info (unPaymentPubKeyHash $ beneficiary2 dat)

        isAfterDeadline :: Bool
        isAfterDeadline = contains (from $ deadline dat) $ txInfoValidRange info

-- el beneficiario uno solo puede retirar antes o en la deadline
-- el beneficiario dos solo puede retirar despues de la deadline
-- wallet 2 = 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7
-- wallet 1 = a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2
-- deadline slot 10 -> posix 1596059101000
-- deadline slot 5 -> posix 1596059096000

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    pkh <- ownPaymentPubKeyHash
    let dat = VestingDatum
                { beneficiary1 = gpBeneficiary gp
                , beneficiary2 = pkh
                , deadline     = gpDeadline gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "This guy is beneficiary1 (pa quien va el regalo) %s" (P.show $ beneficiary1 dat)
    logInfo @P.String $ printf "This guy is beneficiary2 (quien hace el regalo) %s" (P.show $ pkh)
    logInfo @P.String $ printf "B1 make a gift to B2 of %d" (gpAmount gp)
    logInfo @P.String $ printf "This is the deadline %s" (P.show $ gpDeadline gp)



grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now    <- currentTime
    pkh    <- ownPaymentPubKeyHash
    utxos  <- utxosAt scrAddress
    let utxos1 = Map.filter (isSuitable $ \dat -> beneficiary1 dat == pkh && now <= deadline dat) utxos
        utxos2 = Map.filter (isSuitable $ \dat -> beneficiary2 dat == pkh && now >  deadline dat) utxos
    logInfo @P.String $ printf "found %d gift(s) to grab for beneficiary 1" (Map.size utxos1)
    logInfo @P.String $ printf "found %d gift(s) to grab for beneficiary 2" (Map.size utxos2)
    logInfo @P.String $ printf "So endpoint caller is is %s" (P.show $ pkh)
    unless (Map.null utxos1) $ do
        let orefs   = fst <$> Map.toList utxos1
            lookups = Constraints.unspentOutputs utxos1 P.<>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] P.<>
                      Constraints.mustValidateIn (to now)
        void $ submitTxConstraintsWith @Void lookups tx
    unless (Map.null utxos2) $ do
        let orefs   = fst <$> Map.toList utxos2
            lookups = Constraints.unspentOutputs utxos2 P.<>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [Constraints.mustSpendScriptOutput oref $ unitRedeemer | oref <- orefs] P.<>
                      Constraints.mustValidateIn (from now)
        void $ submitTxConstraintsWith @Void lookups tx
  where
    isSuitable :: (VestingDatum -> Bool) -> ChainIndexTxOut -> Bool
    isSuitable p o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum d) -> maybe False p $ PlutusTx.fromBuiltinData d

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
