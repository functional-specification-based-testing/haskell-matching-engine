module Domain.MEService (requestHandler) where

import           Data.Map

import           Decorators.CreditLimit
import           Decorators.FillAndKill
import           Decorators.MinQuantity
import           Decorators.OrderHandler
import           Decorators.AuctionHandler
import           Decorators.Ownership
import           Decorators.PriceBand
import           Decorators.Validation
import           Domain.ME
import           Domain.Matching
import           Infra.Coverage
import           Infra.Decorator


handlerSeed :: Handler
handlerSeed NewOrderRq {} s = NewOrderRs Accepted [] s `covers` "NO-RCV"

handlerSeed ReplaceOrderRq {} s = ReplaceOrderRs Accepted Nothing [] s `covers` "RO-RCV"

handlerSeed CancelOrderRq {} s = CancelOrderRs Accepted Nothing s `covers` "CO-RCV"

handlerSeed ChangeMatchingTypeRq {} s = ChangeMatchingTypeRs Accepted [] s `covers` "CMT-RCV"


continuousNewOrderHandler :: Handler
continuousNewOrderHandler =
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


continuousReplaceOrderHandler :: Handler
continuousReplaceOrderHandler =
    creditLimitProc $
    fillAndKillProc $
    pricebandCheck $
    ownershipCheck $
    orderHandlerDecorator $
    validateOrder
    handlerSeed


auctionArrivingOrderHandler :: Handler
auctionArrivingOrderHandler =
    creditLimitProc $
    pricebandCheck $
    ownershipCheck $
    orderHandlerDecorator $
    validateOrder
    handlerSeed


openingHandler :: Handler
openingHandler =
    creditLimitProc $
    ownershipCheck $
    auctionHandlerDecorator
    handlerSeed

requestHandler :: Handler
requestHandler rq@NewOrderRq {} s
    | isAuction s = auctionArrivingOrderHandler rq s
    | otherwise = continuousNewOrderHandler rq s

requestHandler rq@CancelOrderRq {} s =
    cancelOrderHandler rq s

requestHandler rq@ReplaceOrderRq {} s
    | isAuction s = auctionArrivingOrderHandler rq s
    | otherwise = continuousReplaceOrderHandler rq s

requestHandler rq@(ChangeMatchingTypeRq newt) s
    | newt == matchingType s = ChangeMatchingTypeRs Rejected [] s `covers` "CMT-RJCT"
    | newt == Auction = ChangeMatchingTypeRs Accepted [] s { matchingType = newt } `covers` "CMT-AUC-ACC"
    | newt == Continuous = do 
        rs <- openingHandler rq s
        let s' = state rs
        rs { state = s' { matchingType = newt } } `covers` "CMT-CON-ACC"

requestHandler (SetCreditRq b c) s = do
    return (SetCreditRs Accepted s { creditInfo = insert b c (creditInfo s) })

requestHandler (SetOwnershipRq sh i) s = do
    return (SetOwnershipRs Accepted s { ownershipInfo = insert sh i (ownershipInfo s) })

requestHandler (SetReferencePriceRq rp) s = do
    return (SetReferencePriceRs Accepted s { referencePrice = rp })

requestHandler (SetTotalSharesRq ts) s = do
    return (SetTotalSharesRs Accepted s { totalShares = ts })

requestHandler (SetStaticPriceBandLowerLimitRq pb) s = do
    return (SetStaticPriceBandLowerLimitRs Accepted s { staticPriceBandLowerLimit = pb })

requestHandler (SetStaticPriceBandUpperLimitRq pb) s = do
    return (SetStaticPriceBandUpperLimitRs Accepted s { staticPriceBandUpperLimit = pb })

requestHandler (SetOwnershipUpperLimitRq ol) s = do
    return (SetOwnershipUpperLimitRs Accepted s { ownershipUpperLimit = ol })

requestHandler (SetTickSizeRq t) s = do
    return (SetTickSizeRs Accepted s { tickSize = t })

requestHandler (SetLotSizeRq l) s = do
    return (SetLotSizeRs Accepted s { lotSize = l })
