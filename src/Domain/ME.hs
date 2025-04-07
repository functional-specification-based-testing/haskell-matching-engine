{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.ME
    ( Order (..)
    , Quantity
    , Price
    , TimeStamp
    , OrderID
    , BrokerID
    , ShareholderID
    , CreditInfo
    , OwnershipInfo
    , Side (..)
    , OrderQueue
    , OrderBook (..)
    , Trade (..)
    , MEState (..)
    , Request (..)
    , Response (..)
    , ResponseStatus (..)
    , initMEState
    , limitOrder
    , icebergOrder
    , setId
    , trade
    , removeOrderFromOrderBook
    , valueTraded
    , queueBySide
    , sameSideQueue
    , reject
    , decQty
    , displayedQty
    , oppositeSideQueue
    , updateOppositeQueueInBook
    , enqueue
    , findOrderFromOrderBookByID
    , setVisibleQty
    , enqueueOrder
    , replaceOrderInPlace
    ) where

import GHC.Generics
import Data.Aeson
import           Control.Exception (assert)
import qualified Data.List         as List
import qualified Data.Map          as Map


type Quantity = Int
type Price = Int
type TimeStamp = Int
type OrderID = Int
type BrokerID = Int
type ShareholderID = Int
type CreditInfo = Map.Map BrokerID Int
type OwnershipInfo = Map.Map ShareholderID Int

data Side = Buy | Sell deriving (Show, Eq, Ord, Generic, FromJSON)

data Order = LimitOrder
    { oid         :: OrderID
    , brid        :: BrokerID
    , shid        :: ShareholderID
    , price       :: Price
    , quantity    :: Quantity
    , side        :: Side
    , minQty      :: Maybe Quantity
    , fillAndKill :: Bool
    } | IcebergOrder
    { oid          :: OrderID
    , brid         :: BrokerID
    , shid         :: ShareholderID
    , price        :: Price
    , quantity     :: Quantity
    , side         :: Side
    , minQty       :: Maybe Quantity
    , fillAndKill  :: Bool
    , disclosedQty :: Quantity
    , visibleQty   :: Quantity
    } | MarketToLimitOrder 
    { oid         :: OrderID
    , brid        :: BrokerID
    , shid        :: ShareholderID
    , quantity    :: Quantity
    , side        :: Side
    , minQty      :: Maybe Quantity
    , fillAndKill :: Bool
    } deriving (Show, Eq, Generic, FromJSON)

type OrderQueue = [Order]

data OrderBook = OrderBook
    { buyQueue  :: OrderQueue
    , sellQueue :: OrderQueue
    } deriving (Show, Eq)


data Trade = Trade
    { priceTraded    :: Price
    , quantityTraded :: Quantity
    , buyId          :: OrderID
    , sellId         :: OrderID
    , buyerShId      :: ShareholderID
    , buyerBrId      :: BrokerID
    , sellerShId     :: ShareholderID
    , sellerBrId     :: BrokerID
    } deriving (Show, Eq)


data MEState = MEState
    { orderBook                 :: OrderBook
    , creditInfo                :: CreditInfo
    , ownershipInfo             :: OwnershipInfo
    , referencePrice            :: Price
    , staticPriceBandLowerLimit :: Float
    , staticPriceBandUpperLimit :: Float
    , totalShares               :: Quantity
    , ownershipUpperLimit       :: Float
    , tickSize                  :: Price
    , lotSize                   :: Quantity
    } deriving (Show, Eq)


initMEState :: MEState
initMEState = MEState (OrderBook [] []) Map.empty Map.empty 10 0.9 0.9 100 0.2 1 1


data Request = NewOrderRq
    { order :: Order
    } | CancelOrderRq
    { rqId    :: OrderID
    , oldOid  :: OrderID
    , oldSide :: Side
    } | ReplaceOrderRq
    { oldOid :: OrderID
    , order  :: Order
    } | SetCreditRq
    { broker :: BrokerID
    , credit :: Int
    } | SetOwnershipRq
    { shareholder :: ShareholderID
    , shares      :: Int
    } | SetReferencePriceRq
    { newReferencePrice :: Price
    } | SetTotalSharesRq
    { newTotalShares :: Quantity
    } | SetStaticPriceBandLowerLimitRq
    { newStaticPriceBandLowerLimit :: Float
    } | SetStaticPriceBandUpperLimitRq
    { newStaticPriceBandUpperLimit :: Float
    } | SetOwnershipUpperLimitRq
    { newOwnershipUpperLimit :: Float
    } | SetTickSizeRq
    { newTickSize :: Price
    } | SetLotSizeRq
    { newLotSize :: Quantity
    } deriving (Show, Eq, Generic, FromJSON)

data ResponseStatus = Accepted | Eliminated | Rejected deriving (Show, Eq)

data Response = NewOrderRs
    { status :: ResponseStatus
    , trades :: [Trade]
    , state  :: MEState
    } | CancelOrderRs
    { status   :: ResponseStatus
    , oldOrder :: Maybe Order
    , state    :: MEState
    } | ReplaceOrderRs
    { status   :: ResponseStatus
    , oldOrder :: Maybe Order
    , trades   :: [Trade]
    , state    :: MEState
    } | SetCreditRs
    { status :: ResponseStatus
    , state  :: MEState
    } | SetOwnershipRs
    { status :: ResponseStatus
    , state  :: MEState
    } | SetReferencePriceRs
    { status :: ResponseStatus
    , state  :: MEState
    } | SetTotalSharesRs
    { status :: ResponseStatus
    , state  :: MEState
    } | SetStaticPriceBandLowerLimitRs
    { status :: ResponseStatus
    , state  :: MEState
    } | SetStaticPriceBandUpperLimitRs
    { status :: ResponseStatus
    , state  :: MEState
    } | SetOwnershipUpperLimitRs
    { status :: ResponseStatus
    , state  :: MEState
    } | SetTickSizeRs
    { status :: ResponseStatus
    , state  :: MEState
    } | SetLotSizeRs
    { status :: ResponseStatus
    , state  :: MEState
    } deriving (Show, Eq)


reject :: Request -> MEState -> Response
reject NewOrderRq {} = NewOrderRs Rejected []

reject ReplaceOrderRq {} = ReplaceOrderRs Rejected Nothing []

reject CancelOrderRq {} = CancelOrderRs Rejected Nothing

reject SetCreditRq {} = SetCreditRs Rejected

reject SetOwnershipRq {} = SetOwnershipRs Rejected

reject SetReferencePriceRq {} = SetReferencePriceRs Rejected

reject SetTotalSharesRq {} = SetTotalSharesRs Rejected

reject SetStaticPriceBandLowerLimitRq {} = SetStaticPriceBandLowerLimitRs Rejected

reject SetStaticPriceBandUpperLimitRq {} = SetStaticPriceBandUpperLimitRs Rejected

reject SetOwnershipUpperLimitRq {} = SetOwnershipUpperLimitRs Rejected

reject SetTickSizeRq {} = SetTickSizeRs Rejected

reject SetLotSizeRq {} = SetLotSizeRs Rejected

valueTraded :: Trade -> Int
valueTraded t = (priceTraded t) * (quantityTraded t)

limitOrder :: OrderID -> BrokerID -> ShareholderID -> Price -> Quantity -> Side -> Maybe Quantity -> Bool -> Order
limitOrder i bi shi p q s m fak =
    assert (i >= 0) $
    assert (p > 0) $
    assert (q > 0) $
    case m of
        { (Just mq) -> assert (mq > 0)
        ; otherwise -> id
        } $
    LimitOrder i bi shi p q s m fak

marketToLimitOrder :: OrderID -> BrokerID -> ShareholderID -> Quantity -> Side -> Maybe Quantity -> Bool -> Order
marketToLimitOrder i bi shi q s m fak =
    assert (i >= 0) $
    assert (q > 0) $
    case m of
        { (Just mq) -> assert (mq > 0)
        ; otherwise -> id
        } $
    MarketToLimitOrder i bi shi q s m fak


icebergOrder :: OrderID -> BrokerID -> ShareholderID -> Price -> Quantity -> Side -> Maybe Quantity -> Bool -> Quantity -> Quantity -> Order
icebergOrder i bi shi p q s m fak dq vq =
    assert (i >= 0) $
    assert (p > 0) $
    assert (q > 0) $
    case m of
        { (Just mq) -> assert (mq > 0)
        ; otherwise -> id
        } $
    assert (dq <= q) $
    assert (vq > 0 && vq <= dq && vq <= q) $
    IcebergOrder i bi shi p q s m fak dq vq

setId :: OrderID -> Order -> Order
setId i o@(LimitOrder _ bi shi p q s m fak) =
    limitOrder i bi shi p q s m fak
setId i o@(MarketToLimitOrder _ bi shi q s m fak) =
    marketToLimitOrder i bi shi q s m fak
setId i o@(IcebergOrder _ bi shi p q s m fak dq vq) =
    icebergOrder i bi shi p q s m fak dq vq

displayedQty :: Order -> Quantity
displayedQty o@IcebergOrder {} = visibleQty o

displayedQty o@LimitOrder {}   = quantity o
displayedQty o@MarketToLimitOrder {}   = quantity o


decQty :: Order -> Quantity -> Order
decQty o@(LimitOrder _ _ _ _ q _ _ _) q' = setQty o $ q - q'
decQty o@(MarketToLimitOrder _ _ _ q _ _ _) q' = setQty o $ q - q'
decQty o@(IcebergOrder _ _ _ _ q _ _ _ _ vq) q' = setQties o (q - q') (vq - q')


setQty :: Order -> Quantity -> Order
setQty (LimitOrder i bi shi p _ s m fak) q' =
    limitOrder i bi shi p q' s m fak
setQty (MarketToLimitOrder i bi shi _ s m fak) q' =
    marketToLimitOrder i bi shi q' s m fak
setQty (IcebergOrder i bi shi p _ s m fak dq vq) q' =
    icebergOrder i bi shi p q' s m fak dq vq


setVisibleQty :: Order -> Quantity -> Order
setVisibleQty (IcebergOrder i bi shi p q s m fak dq _) vq' =
    icebergOrder i bi shi p q s m fak dq vq'


setQties :: Order -> Quantity -> Quantity -> Order
setQties (IcebergOrder i bi shi p _ s m fak dq _) q' vq' =
    icebergOrder i bi shi p q' s m fak dq vq'


removeOrderFromQueue :: Order -> OrderQueue -> OrderQueue
removeOrderFromQueue = List.deleteBy (\ o1 o2 -> oid o1 == oid o2)


replaceOrderInQueue :: OrderID -> Order -> OrderQueue -> OrderQueue
replaceOrderInQueue _ _ [] = []

replaceOrderInQueue ooid o (h:t)
    | oid h == ooid = o:replaceOrderInQueue ooid o t
    | otherwise     = h:replaceOrderInQueue ooid o t


findOrderFromQueueByID :: OrderID -> OrderQueue -> Maybe Order
findOrderFromQueueByID oidToFind oq
    | null filtered = Nothing
    | otherwise     = Just $ head filtered
  where
    filtered = List.filter (\o -> oid o == oidToFind) oq


applyOnQueue :: (OrderQueue -> OrderQueue) -> Side -> OrderBook -> OrderBook
applyOnQueue f side (OrderBook bq sq)
    | side == Buy  = OrderBook (f bq) sq
    | side == Sell = OrderBook bq (f sq)
    | otherwise    = error "invalid Side"


applyOnSameSideQueue :: (OrderQueue -> OrderQueue) -> Order -> OrderBook -> OrderBook
applyOnSameSideQueue f o = applyOnQueue f (side o)


queueBySide :: Side -> OrderBook -> OrderQueue
queueBySide side ob
    | side == Buy  = buyQueue ob
    | side == Sell = sellQueue ob
    | otherwise    = error "invalid Side"


sameSideQueue :: Order -> OrderBook -> OrderQueue
sameSideQueue o = queueBySide $ side o


oppositeSideQueue :: Order -> OrderBook -> OrderQueue
oppositeSideQueue o = queueBySide os
  where
    os = case side o of
        Buy       -> Sell
        Sell      -> Buy


removeOrderFromOrderBook :: Order -> OrderBook -> OrderBook
removeOrderFromOrderBook o = applyOnSameSideQueue (removeOrderFromQueue o) o


replaceOrderInOrderBook :: OrderID -> Order -> OrderBook -> OrderBook
replaceOrderInOrderBook ooid o = applyOnSameSideQueue (replaceOrderInQueue ooid o) o


findOrderFromOrderBookByID :: OrderID -> Side -> OrderBook ->  Maybe Order
findOrderFromOrderBookByID oid side ob = findOrderFromQueueByID oid $ queueBySide side ob


queuesBefore :: Order -> Order -> Bool
queuesBefore o o'
    | side o == Sell && side o' == Sell = price o < price o'
    | side o == Buy  && side o' == Buy  = price o > price o'
    | otherwise = error "incomparable orders"


enqueueOrder :: Order -> OrderQueue -> OrderQueue
enqueueOrder o@LimitOrder {} = enqueueOrder' o

enqueueOrder o@(IcebergOrder _ _ _ _ q _ _ _ dq _) = enqueueOrder' $ setVisibleQty o $ min q dq


enqueueOrder' :: Order -> OrderQueue -> OrderQueue
enqueueOrder' o [] = [o]

enqueueOrder' o (o1:os)
    | queuesBefore o o1 = o:(o1:os)
    | otherwise = o1:enqueueOrder' o os


enqueue :: Maybe Order -> OrderBook -> OrderBook
enqueue Nothing {} = id

enqueue (Just o)   = applyOnSameSideQueue (enqueueOrder o) o


trade :: Price -> Quantity -> Order -> Order -> Trade
trade p q newo oppositeo
    | side newo == Buy  = Trade p q newi headi newshi newbi headshi headbi
    | side newo == Sell = Trade p q headi newi headshi headbi newshi newbi
  where
    newi = oid newo
    newshi = shid newo
    newbi = brid newo
    headi = oid oppositeo
    headshi = shid oppositeo
    headbi = brid oppositeo


updateOppositeQueueInBook :: Order -> OrderQueue -> OrderBook -> OrderBook
updateOppositeQueueInBook o oq ob
    | side o == Buy  = ob{sellQueue = oq}
    | side o == Sell = ob{buyQueue = oq}
    | otherwise = error "invalid Side"


replaceOrderInPlace :: OrderID -> Order -> OrderBook -> (OrderBook, [Trade])
replaceOrderInPlace ooid o ob = (replaceOrderInOrderBook ooid o ob, [])
