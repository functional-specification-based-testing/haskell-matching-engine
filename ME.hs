module ME
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
    , OrderResponseStatus (..)
    , Handler
    , Decorator
    , initMEState
    , limitOrder
    , icebergOrder
    , removeOrderFromOrderBook
    , valueTraded
    , queue
    , matchNewOrder
    , cancelOrder
    , adjustPeakSizeOnReplace
    , shouldReplaceInPlace
    , replaceOrderInPlace
    ) where
import           Control.Exception (assert)
import           Coverage
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

data Side = Buy | Sell deriving (Show, Eq, Ord)

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
    } deriving (Show, Eq)

type OrderQueue = [Order]

data OrderBook = OrderBook
    { buyQueue  :: OrderQueue
    , sellQueue :: OrderQueue
    } deriving (Show, Eq)


queue :: Side -> OrderBook -> OrderQueue
queue Buy ob  = buyQueue ob
queue Sell ob = sellQueue ob


data Trade = Trade
  { priceTraded      :: Price
    , quantityTraded :: Quantity
    , buyId          :: OrderID
    , sellId         :: OrderID
    , buyerShId      :: ShareholderID
    , buyerBrId      :: BrokerID
    , sellerShId     :: ShareholderID
    , sellerBrId     :: BrokerID
    } deriving (Show, Eq)


data MEState = MEState
    { orderBook      :: OrderBook
    , creditInfo     :: CreditInfo
    , ownershipInfo  :: OwnershipInfo
    , referencePrice :: Price
    } deriving (Show, Eq)


initMEState :: MEState
initMEState = MEState (OrderBook [] []) Map.empty Map.empty 0


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
    { newReferencePrice :: Int
    } deriving (Show, Eq)

data OrderResponseStatus = Accepted | Rejected deriving (Show, Eq)

data Response = NewOrderRs
    { status :: OrderResponseStatus
    , trades :: [Trade]
    } | CancelOrderRs
    { status   :: OrderResponseStatus
    , oldOrder :: Maybe Order
    } | ReplaceOrderRs
    { status   :: OrderResponseStatus
    , oldOrder :: Maybe Order
    , trades   :: [Trade]
    } | SetCreditRs
    { success :: Bool
    } | SetOwnershipRs
    { success :: Bool
    } | SetReferencePriceRs
    { success :: Bool
    } deriving (Show, Eq)


type Handler = Request -> MEState -> Coverage (Response, MEState)
type Decorator = Handler -> Handler


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


isIceberg :: Order -> Bool
isIceberg IcebergOrder {} = True

isIceberg LimitOrder {}   = False


displayedQty :: Order -> Quantity
displayedQty o@IcebergOrder {} = visibleQty o

displayedQty o@LimitOrder {}   = quantity o


decQty :: Order -> Quantity -> Order
decQty o@(LimitOrder _ _ _ _ q _ _ _) q' = setQty o $ q - q'

decQty o@(IcebergOrder _ _ _ _ q _ _ _ _ vq) q' = setQties o (q - q') (vq - q')


setQty :: Order -> Quantity -> Order
setQty (LimitOrder i bi shi p _ s m fak) q' =
    limitOrder i bi shi p q' s m fak

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
    | oid h == ooid = o:t
    | otherwise     = h:t


findOrderFromQueueByID :: OrderID -> OrderQueue -> Maybe Order
findOrderFromQueueByID oidToRemove oq
    | null filtered = Nothing
    | otherwise     = Just $ head filtered
  where
    filtered = List.filter (\o -> oid o == oidToRemove) oq


removeOrderFromOrderBook :: Order -> OrderBook -> OrderBook
removeOrderFromOrderBook o (OrderBook bq sq)
    | side o == Buy  = OrderBook (removeOrderFromQueue o bq) sq
    | side o == Sell = OrderBook bq (removeOrderFromQueue o sq)
    | otherwise = error "incomparable orders"


replaceOrderInOrderBook :: OrderID -> Order -> OrderBook -> OrderBook
replaceOrderInOrderBook ooid o (OrderBook bq sq)
    | side o == Buy  = OrderBook (replaceOrderInQueue ooid o bq) sq
    | side o == Sell = OrderBook bq (replaceOrderInQueue ooid o sq)
    | otherwise = error "incomparable orders"


findOrderFromOrderBookByID :: OrderID -> Side -> OrderBook ->  Maybe Order
findOrderFromOrderBookByID oid side (OrderBook bq sq)
    | side == Buy  = findOrderFromQueueByID oid bq
    | side == Sell = findOrderFromQueueByID oid sq
    | otherwise = error "incomparable orders"


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


enqueue :: Order -> OrderBook -> OrderBook
enqueue o ob
    | side o == Buy  = OrderBook (enqueueOrder o $ buyQueue ob) (sellQueue ob)
    | side o == Sell = OrderBook (buyQueue ob) (enqueueOrder o $ sellQueue ob)
    | otherwise = error "incomparable orders"


enqueueRemainder :: OrderQueue -> Order -> Coverage OrderQueue
enqueueRemainder os o@LimitOrder {}
    | q == 0 = os `covers` "ELR-1"
    | otherwise = enqueueOrder o os `covers` "ELR-2"
  where
    q = quantity o

enqueueRemainder os o@IcebergOrder {}
    | q == 0 = os `covers` "EIR-1"
    | vq == 0 && q <= dq = enqueueOrder (setVisibleQty o q) os `covers` "EIR-2"
    | vq == 0 && q > dq = enqueueOrder (setVisibleQty o dq) os `covers` "EIR-3"
    | otherwise = enqueueOrder o os `covers` "EIR-4"
  where
    q = quantity o
    vq = visibleQty o
    dq = disclosedQty o


matchBuy :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
matchBuy o [] = (Just o, [], []) `covers` "MB-0"

matchBuy o oq@(h:os)
    | newp < headp = (Just o, oq, []) `covers` "MB-1"
    | newq < headq = (Nothing, (decQty h newq):os, [Trade headp newq newi headi newshi newbi headshi headbi]) `covers` "MB-2"
    | newq == headq = do
        newQueue <- enqueueRemainder os $ decQty h newq
        (Nothing, newQueue, [Trade headp newq newi headi newshi newbi headshi headbi]) `covers` "MB-3"
    | newq > headq = do
        newQueue <- enqueueRemainder os $ decQty h headq
        (o', ob', ts') <- matchBuy (decQty o headq) newQueue
        (o', ob', (Trade headp headq newi headi newshi newbi headshi headbi):ts') `covers` "MB-4"
  where
    newp = price o
    newq = quantity o
    newi = oid o
    newshi = shid o
    newbi = brid o
    headp = price h
    headq = displayedQty h
    headi = oid h
    headshi = shid h
    headbi = brid h


matchSell :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
matchSell o [] = (Just o, [], []) `covers` "MS-0"

matchSell o oq@(h:os)
    | newp > headp = (Just o, oq, []) `covers` "MS-1"
    | newq < headq = (Nothing, (decQty h newq):os, [Trade headp newq headi newi headshi headbi newshi newbi]) `covers` "MS-2"
    | newq == headq = do
        newQueue <- enqueueRemainder os $ decQty h newq
        (Nothing, newQueue, [Trade headp newq headi newi headshi headbi newshi newbi]) `covers` "MS-3"
    | newq > headq = do
        newQueue <- enqueueRemainder os $ decQty h headq
        (o', ob', ts') <- matchBuy (decQty o headq) newQueue
        (o', ob', (Trade headp headq headi newi headshi headbi newshi newbi):ts') `covers` "MS-4"
  where
    newp = price o
    newq = quantity o
    newi = oid o
    newshi = shid o
    newbi = brid o
    headp = price h
    headq = displayedQty h
    headi = oid h
    headshi = shid h
    headbi = brid h


matchNewOrder :: Order -> OrderBook -> Coverage (OrderBook, [Trade])
matchNewOrder o ob
    | side o == Buy = do
        (rem, sq, ts) <- (matchBuy o (sellQueue ob))
        case rem of
            Nothing -> (OrderBook (buyQueue ob) sq, ts) `covers` "MNO-1"
            Just o' -> (enqueue o' $ OrderBook (buyQueue ob) sq, ts) `covers` "MNO-2"
    | side o == Sell = do
        (rem, bq, ts) <- (matchSell o (buyQueue ob))
        case rem of
            Nothing -> (OrderBook bq (sellQueue ob), ts) `covers` "MNO-3"
            Just o' -> (enqueue o' $ OrderBook bq (sellQueue ob), ts) `covers` "MNO-4"


cancelOrder :: OrderID -> Side -> OrderBook -> Coverage (OrderBook, Maybe Order)
cancelOrder oid side ob = do
    case findOrderFromOrderBookByID oid side ob of
        Just o -> (ob', Just o) `covers` "CO-1"
          where
            ob' = removeOrderFromOrderBook o ob
        Nothing -> (ob, Nothing) `covers` "CO-2"


replaceOrderInPlace :: OrderID -> Order -> OrderBook -> Coverage (OrderBook, [Trade])
replaceOrderInPlace ooid o ob = (replaceOrderInOrderBook ooid o ob, []) `covers` "ROIP-1"


shouldReplaceInPlace :: Order -> Order -> Bool
shouldReplaceInPlace oldOrder order
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
