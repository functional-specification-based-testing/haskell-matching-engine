module Domain.MEService (requestHandler) where

import           Data.Map

import           Decorators.CreditLimit
import           Decorators.FillAndKill
import           Decorators.MinQuantity
import           Decorators.OrderHandler
import           Decorators.Ownership
import           Decorators.PriceBand
import           Decorators.Validation
import           Domain.ME
-- import           Infra.Coverage
import           Infra.Decorator


handlerSeed :: Handler
handlerSeed NewOrderRq {} s = NewOrderRs Accepted [] s

handlerSeed ReplaceOrderRq {} s = ReplaceOrderRs Accepted Nothing [] s

handlerSeed CancelOrderRq {} s = CancelOrderRs Accepted Nothing s 


newOrderHandler :: Handler
newOrderHandler =
    creditLimitProc $
    fillAndKillProc $
    minQuantityCheck $
    pricebandCheck $
    ownershipCheck $
    orderHandlerDecorator $
    validateOrder
    handlerSeed


cancelOrderHandler :: Handler
cancelOrderHandler =
    creditLimitProc $
    pricebandCheck $
    ownershipCheck $
    orderHandlerDecorator $
    validateOrder
    handlerSeed


replaceOrderHandler :: Handler
replaceOrderHandler =
    creditLimitProc $
    fillAndKillProc $
    pricebandCheck $
    ownershipCheck $
    orderHandlerDecorator $
    validateOrder
    handlerSeed


requestHandler :: Handler
requestHandler rq@NewOrderRq {} s =
    newOrderHandler rq s

requestHandler rq@CancelOrderRq {} s =
    cancelOrderHandler rq s

requestHandler rq@ReplaceOrderRq {} s =
    replaceOrderHandler rq s

requestHandler (SetCreditRq b c) s = do
    (SetCreditRs Accepted s { creditInfo = insert b c (creditInfo s) })

requestHandler (SetOwnershipRq sh i) s = do
    (SetOwnershipRs Accepted s { ownershipInfo = insert sh i (ownershipInfo s) })

requestHandler (SetReferencePriceRq rp) s = do
    (SetReferencePriceRs Accepted s { referencePrice = rp })

requestHandler (SetTotalSharesRq ts) s = do
    (SetTotalSharesRs Accepted s { totalShares = ts })

requestHandler (SetStaticPriceBandLowerLimitRq pb) s = do
    (SetStaticPriceBandLowerLimitRs Accepted s { staticPriceBandLowerLimit = pb })

requestHandler (SetStaticPriceBandUpperLimitRq pb) s = do
    (SetStaticPriceBandUpperLimitRs Accepted s { staticPriceBandUpperLimit = pb })

requestHandler (SetOwnershipUpperLimitRq ol) s = do
    (SetOwnershipUpperLimitRs Accepted s { ownershipUpperLimit = ol })

requestHandler (SetTickSizeRq t) s = do
    (SetTickSizeRs Accepted s { tickSize = t })

requestHandler (SetLotSizeRq l) s = do
    (SetLotSizeRs Accepted s { lotSize = l })
