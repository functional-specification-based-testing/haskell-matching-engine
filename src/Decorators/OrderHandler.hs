module Decorators.OrderHandler (orderHandlerDecorator) where

import           Data.Maybe

import           Decorators.Validation
import           Domain.ME
import           Infra.Coverage
import           Infra.Decorator


newOrderMatcher :: Handler
newOrderMatcher (NewOrderRq o) s = do
    (ob, ts) <- matchNewOrder o (orderBook s)
    return (NewOrderRs Accepted ts s { orderBook = ob})


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
    if isNothing oldo
        then return $ reject rq s
        else do
            let oldOrder = fromJust oldo
            if postponedCheckOnReplace oldOrder oNotAdjusted
                then do
                    let o = adjustPeakSizeOnReplace oldOrder oNotAdjusted
                    (ob'', ts) <- if shouldSubstituteOrder oldOrder o then substituteOrder (oid oldOrder) o ob else matchNewOrder o ob'
                    return (ReplaceOrderRs Accepted oldo ts s { orderBook = ob''})
                else return $ reject rq s


substituteOrder :: OrderID -> Order -> OrderBook -> Coverage (OrderBook, [Trade])
substituteOrder ooid o ob = (replaceOrderInPlace ooid o ob) `covers` ("ROIP-1 DF-U-ob-" ++ show ooid ++ " DF-D-ob-" ++ show (oid o))


orderHandlerDecorator :: Decorator
orderHandlerDecorator =
    decorateOnAccept "PRC-" orderHandlerDecoratorOnAccept


orderHandlerDecoratorOnAccept :: PartialDecorator
orderHandlerDecoratorOnAccept rq@NewOrderRq{} s _ = do
    newOrderMatcher rq s

orderHandlerDecoratorOnAccept rq@ReplaceOrderRq {} s _ = do
    orderReplacer rq s

orderHandlerDecoratorOnAccept rq@CancelOrderRq {} s _ = do
    orderCanceller rq s


canBeMatchedWithOppositeQueueHead :: Order -> Order -> Bool
canBeMatchedWithOppositeQueueHead o h
    | s == Buy  = newp >= headp
    | s == Sell = newp <= headp
  where
    s = side o
    newp = price o
    headp = price h


match :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
match o [] = (Just o, [], []) `covers` "M-0"

match o oq@(h:os)
    | not $ canBeMatchedWithOppositeQueueHead o h = (Just o, oq, []) `covers` ("M-1 DF-U-ob-" ++ show qid )
    | newq < headq = (Nothing, (decQty h newq):os, [trade headp newq o h]) `covers` ("M-2 DF-U-ob-" ++ show qid ++ " DF-D-ob-" ++ show qid)
    | newq == headq = do
        newQueue <- enqueueRemainder os $ decQty h newq
        (Nothing, newQueue, [trade headp newq o h]) `covers` "M-3"
    | newq > headq = do
        newQueue <- (enqueueRemainder os $ decQty h headq) 
        (o', oq', ts') <- match (decQty o headq) newQueue
        (o', oq', (trade headp headq o h):ts') `covers` "M-4"
  where
    id = oid o
    qid = oid h
    newq = quantity o
    headp = price h
    headq = displayedQty h


matchNewOrder :: Order -> OrderBook -> Coverage (OrderBook, [Trade])
matchNewOrder o ob = do
    let oq = oppositeSideQueue o ob
    (remo, oq', ts) <- match o oq
    let ob'=  updateOppositeQueueInBook o oq' ob 
    let ob'' = enqueue remo ob' 
    case remo of 
        Just remainder -> (ob'', ts) `covers` ("DF-D-ob-" ++ show (oid remainder))
        _ -> (ob'', ts) `covers` "DF-tau"
    -- (ob'', ts) `covers` "MNO"


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


enqueueRemainder :: OrderQueue -> Order -> Coverage OrderQueue
enqueueRemainder os o@LimitOrder {}
    | q == 0 = os `covers` ("ELR-1 DF-U-ob-" ++ show id)
    | otherwise = enqueueOrder o os `covers` ("ELR-2 DF-U-ob-" ++ show id ++ " DF-D-ob-" ++ show id)
  where
    q = quantity o
    id = oid o

enqueueRemainder os o@IcebergOrder {}
    | q == 0 = os `covers` ("EIR-1 DF-U-ob-" ++ show id)
    | vq == 0 && q <= dq = enqueueOrder (setVisibleQty o q) os `covers` ("EIR-2 DF-U-ob-" ++ show id ++ " DF-D-ob-" ++ show id)
    | vq == 0 && q > dq = enqueueOrder (setVisibleQty o dq) os `covers` ("EIR-3 DF-U-ob-" ++ show id ++ " DF-D-ob-" ++ show id)
    | otherwise = enqueueOrder o os `covers` ("EIR-4 DF-U-ob-" ++ show id ++ " DF-D-ob-" ++ show id)
  where
    id = oid o
    q = quantity o
    vq = visibleQty o
    dq = disclosedQty o
