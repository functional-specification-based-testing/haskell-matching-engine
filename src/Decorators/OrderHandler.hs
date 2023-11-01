module Decorators.OrderHandler (orderHandlerDecorator) where

import           Data.Maybe

import           Decorators.Validation
import           Domain.ME
-- import           Infra.Coverage
import           Infra.Decorator


newOrderMatcher :: Handler
newOrderMatcher !(NewOrderRq o) !s = do
    let !(ob, ts) = matchNewOrder o (orderBook s)
    (NewOrderRs Accepted ts s { orderBook = ob})


orderCanceller :: Handler
orderCanceller !(CancelOrderRq _ oid side) !s = do
    let !ob = orderBook s
    let !(ob', o) = cancelOrder oid side ob
    let !status = if isNothing o then Rejected else Accepted
    (CancelOrderRs status o s { orderBook = ob'})


orderReplacer :: Handler
orderReplacer !rq@(ReplaceOrderRq oldoid oNotAdjusted) !s = do
    let !ob = orderBook s
    let !(ob', oldo) = cancelOrder oldoid (side oNotAdjusted) ob
    if isNothing oldo
        then  reject rq s
        else do
            let !oldOrder = fromJust oldo
            if postponedCheckOnReplace oldOrder oNotAdjusted
                then do
                    let !o = adjustPeakSizeOnReplace oldOrder oNotAdjusted
                    let !(ob'', ts) = if shouldSubstituteOrder oldOrder o then substituteOrder (oid oldOrder) o ob else matchNewOrder o ob'
                    (ReplaceOrderRs Accepted oldo ts s { orderBook = ob''})
                else reject rq s


substituteOrder :: OrderID -> Order -> OrderBook ->  (OrderBook, [Trade])
substituteOrder !ooid !o !ob = (replaceOrderInPlace ooid o ob) 


orderHandlerDecorator :: Decorator
orderHandlerDecorator =
    decorateOnAccept  orderHandlerDecoratorOnAccept


orderHandlerDecoratorOnAccept :: PartialDecorator
orderHandlerDecoratorOnAccept !rq@NewOrderRq{} !s _ = do
    newOrderMatcher rq s

orderHandlerDecoratorOnAccept !rq@ReplaceOrderRq {} !s _ = do
    orderReplacer rq s

orderHandlerDecoratorOnAccept !rq@CancelOrderRq {} !s _ = do
    orderCanceller rq s


canBeMatchedWithOppositeQueueHead :: Order -> Order -> Bool
canBeMatchedWithOppositeQueueHead !o !h
    | s == Buy  = newp >= headp
    | s == Sell = newp <= headp
  where
    !s = side o
    !newp = price o
    !headp = price h


match :: Order -> OrderQueue ->  (Maybe Order, OrderQueue, [Trade])
match !o [] = (Just o, [], []) 

match !o !oq@(h:os)
    | not $ canBeMatchedWithOppositeQueueHead o h = (Just o, oq, []) 
    | newq < headq = (Nothing, (decQty h newq):os, [trade headp newq o h]) 
    | newq == headq = do
        let !newQueue = enqueueRemainder os $ decQty h newq
        (Nothing, newQueue, [trade headp newq o h])
    | newq > headq = do
        let !newQueue = enqueueRemainder os $ decQty h headq
        let !(o', oq', ts') = match (decQty o headq) newQueue
        (o', oq', (trade headp headq o h):ts')
  where
    newq = quantity o
    headp = price h
    headq = displayedQty h


matchNewOrder :: Order -> OrderBook ->  (OrderBook, [Trade])
matchNewOrder !o !ob = do
    let !oq = oppositeSideQueue o ob
    let !(remo, oq', ts) = match o oq
    let !ob' = updateOppositeQueueInBook o oq' ob
    let !ob'' = enqueue remo ob'
    (ob'', ts)

cancelOrder :: OrderID -> Side -> OrderBook ->  (OrderBook, Maybe Order)
cancelOrder !oid !side !ob = do
    case findOrderFromOrderBookByID oid side ob of
        Just o -> (ob', Just o)
          where
            !ob' = removeOrderFromOrderBook o ob
        Nothing -> (ob, Nothing) 


shouldSubstituteOrder :: Order -> Order -> Bool
shouldSubstituteOrder !oldOrder !order
    | displayedQty order > displayedQty oldOrder = False
    | price order /= price oldOrder = False
    | otherwise = True


adjustPeakSizeOnReplace :: Order -> Order -> Order
adjustPeakSizeOnReplace !oldOrder@LimitOrder {} !notAdjustedNewOrder = notAdjustedNewOrder

adjustPeakSizeOnReplace oldOrder@IcebergOrder {} !notAdjustedNewOrder@LimitOrder {} = notAdjustedNewOrder

adjustPeakSizeOnReplace !oldOrder@IcebergOrder {} !notAdjustedNewOrder@IcebergOrder {}
    | oldvq == olddq = setVisibleQty notAdjustedNewOrder newdq
    | oldvq < olddq && oldvq > newdq = setVisibleQty notAdjustedNewOrder newdq
    | otherwise = notAdjustedNewOrder
  where
    !olddq = disclosedQty oldOrder
    !newdq = disclosedQty notAdjustedNewOrder
    !oldvq = visibleQty oldOrder


enqueueRemainder :: OrderQueue -> Order ->  OrderQueue
enqueueRemainder !os !o@LimitOrder {}
    | q == 0 = os 
    | otherwise = enqueueOrder o os 
  where
    !q = quantity o

enqueueRemainder !os !o@IcebergOrder {}
    | q == 0 = os 
    | vq == 0 && q <= dq = enqueueOrder (setVisibleQty o q) os 
    | vq == 0 && q > dq = enqueueOrder (setVisibleQty o dq) os 
    | otherwise = enqueueOrder o os
  where
    !q = quantity o
    !vq = visibleQty o
    !dq = disclosedQty o
