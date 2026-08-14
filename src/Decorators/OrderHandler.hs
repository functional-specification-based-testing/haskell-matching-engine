module Decorators.OrderHandler (orderHandlerDecorator) where

import           Data.Maybe

import           Decorators.Validation
import           Domain.ME
import           Domain.Matching
import           Infra.Coverage
import           Infra.Decorator


newOrderHandler :: Handler
newOrderHandler (NewOrderRq o) s 
    | not $ isAuction s = do
        (ob, ts) <- continuousMatch o (orderBook s)
        return (NewOrderRs Accepted ts s { orderBook = ob})
    | otherwise = do 
        let ob = enqueue (Just o) (orderBook s)
        return (NewOrderRs Accepted [] s { orderBook = ob })


orderCanceller :: Handler
orderCanceller (CancelOrderRq _ oid side) s = do
    let ob = orderBook s
    (ob', o) <- cancelOrder oid side ob
    let status = if isNothing o then Rejected else Accepted
    return (CancelOrderRs status o s { orderBook = ob'})


orderReplacer :: Handler
orderReplacer rq@(ReplaceOrderRq oldoid oNotAdjusted) s = do
    let ob = orderBook s
    (ob', oldo) <- cancelOrder oldoid (side oNotAdjusted) ob
    case oldo of
        Nothing -> return $ reject rq s
        Just oldOrder
            | not (postponedCheckOnReplace oldOrder oNotAdjusted) -> return $ reject rq s
            | matchingType s == Continuous -> do
                let o = adjustPeakSizeOnReplace oldOrder oNotAdjusted
                (ob'', ts) <- if shouldSubstituteOrder oldOrder o then substituteOrder (oid oldOrder) o ob else continuousMatch o ob'
                return (ReplaceOrderRs Accepted oldo ts s { orderBook = ob'' })
            | otherwise -> do
                ob'' <-
                    if shouldSubstituteOrder oldOrder oNotAdjusted
                        then do
                            (obSubstituted, _) <- substituteOrder (oid oldOrder) oNotAdjusted ob
                            return obSubstituted
                        else
                            return (enqueue (Just oNotAdjusted) ob')
                return (ReplaceOrderRs Accepted oldo [] s { orderBook = ob'' })


substituteOrder :: OrderID -> Order -> OrderBook -> Coverage (OrderBook, [Trade])
substituteOrder ooid o ob = (replaceOrderInPlace ooid o ob) `covers` ("ROIP-1 DF-U-ob-" ++ show ooid ++ " DF-D-ob-" ++ show (oid o))


orderHandlerDecorator :: Decorator
orderHandlerDecorator =
    decorateOnAccept "PRC-" orderHandlerDecoratorOnAccept


orderHandlerDecoratorOnAccept :: PartialDecorator
orderHandlerDecoratorOnAccept rq@NewOrderRq{} s _ = do
    newOrderHandler rq s

orderHandlerDecoratorOnAccept rq@ReplaceOrderRq {} s _ = do
    orderReplacer rq s

orderHandlerDecoratorOnAccept rq@CancelOrderRq {} s _ = do
    orderCanceller rq s


cancelOrder :: OrderID -> Side -> OrderBook -> Coverage (OrderBook, Maybe Order)
cancelOrder oid side ob = do
    case findOrderFromOrderBookByID oid side ob of
        Just o -> (ob', Just o) `covers` ("CO-1 DF-U-ob-" ++ show oid)
          where
            ob' = removeOrderFromOrderBook o ob
        Nothing -> (ob, Nothing) `covers` "CO-2"


shouldSubstituteOrder :: Order -> Order -> Bool
shouldSubstituteOrder oldOrder order
    | displayedQty order > displayedQty oldOrder = False
    | price order /= price oldOrder = False
    | otherwise = True


adjustPeakSizeOnReplace :: Order -> Order -> Order
adjustPeakSizeOnReplace oldOrder@LimitOrder {} notAdjustedNewOrder = notAdjustedNewOrder

adjustPeakSizeOnReplace oldOrder@IcebergOrder {} notAdjustedNewOrder@LimitOrder {} = notAdjustedNewOrder

adjustPeakSizeOnReplace oldOrder@IcebergOrder {} notAdjustedNewOrder@IcebergOrder {}
    | oldvq == olddq = setVisibleQty notAdjustedNewOrder newdq
    | oldvq < olddq && oldvq > newdq = setVisibleQty notAdjustedNewOrder newdq
    | otherwise = notAdjustedNewOrder
  where
    olddq = disclosedQty oldOrder
    newdq = disclosedQty notAdjustedNewOrder
    oldvq = visibleQty oldOrder

