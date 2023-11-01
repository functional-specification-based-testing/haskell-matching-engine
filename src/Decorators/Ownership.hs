module Decorators.Ownership (ownershipCheck) where

import           Data.Map

import           Domain.ME
-- import           Infra.Coverage
import           Infra.Decorator


ownershipCheck :: Decorator
ownershipCheck =
    decorateOnAccept  $ ownershipCheckByType


ownershipCheckByType :: PartialDecorator
ownershipCheckByType !rq@NewOrderRq {} !s !rs =
    ownershipCheckForArrivingOrder rq s rs

ownershipCheckByType !rq@ReplaceOrderRq {} !s !rs =
    ownershipCheckForArrivingOrder rq s rs

ownershipCheckByType _ _ !rs = rs 


getOldOrder :: Response -> Maybe Order
getOldOrder !rs@ReplaceOrderRs {} =
    oldOrder rs

getOldOrder !rs@CancelOrderRs {} =
    oldOrder rs

getOldOrder _ =
    Nothing


ownershipCheckForArrivingOrder :: PartialDecorator
ownershipCheckForArrivingOrder !rq !s !rs = do
    let !o = order rq
    let !oldo = getOldOrder rs
    let !s' = state rs
    let !maxOwnershipPortion = ownershipUpperLimit s
    if ownershipPreCheck maxOwnershipPortion o oldo s
        then rs { state = updateOwnershipInfo (trades rs) s' } 
        else reject rq s 


updateOwnershipInfo :: [Trade] -> MEState -> MEState
updateOwnershipInfo !ts !state =
    state {ownershipInfo = Prelude.foldl updateOwnership oi ts}
  where
    !oi = ownershipInfo state


updateOwnership :: OwnershipInfo -> Trade -> OwnershipInfo
updateOwnership !oi !t =
    adjust (+ q) bshid $! adjust (subtract q) sshid oi
  where
    !q = quantityTraded t
    !bshid = buyerShId t
    !sshid = sellerShId t


quantityInBook :: Maybe Order -> OrderBook -> Int
quantityInBook !(Just o) !ob =
    quantityInQueue o $ sameSideQueue o ob

quantityInBook Nothing _ = 0


quantityInQueue :: Order -> OrderQueue -> Int
quantityInQueue !o !q =
    sum $!
    Prelude.map quantity $!
    Prelude.filter (\orderInQueue -> oid orderInQueue == oid o) $!
    q


ownershipPreCheck :: Float -> Order -> Maybe Order -> MEState -> Bool
ownershipPreCheck !maxOwnershipPortion !o !oldOrder !state = do
    shi `member` ownership && case side o of
        Buy  -> ownedQty + newOrderQty + bookedBuyQty - bookedOrderQty < maxOwnership
        Sell -> newOrderQty + bookedSellQty - bookedOrderQty <= ownedQty
  where
    !shi = shid o
    !ownership = ownershipInfo state
    !ownedQty = ownership!shi
    !ob = orderBook state
    !newOrderQty = quantity o
    !bookedOrderQty = quantityInBook oldOrder ob
    !bookedBuyQty = totalQuantityInQueue Buy shi ob
    !bookedSellQty = totalQuantityInQueue Sell shi ob
    !shares = totalShares state
    !maxOwnership = floor $! fromIntegral shares * maxOwnershipPortion


totalQuantityInQueue :: Side -> ShareholderID -> OrderBook -> Quantity
totalQuantityInQueue !side !shi !ob =
    sum $!
    Prelude.map quantity $!
    Prelude.filter (\o -> shid o == shi) $!
    queueBySide side ob
