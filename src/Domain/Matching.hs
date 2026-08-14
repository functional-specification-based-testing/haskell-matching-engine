module Domain.Matching
    ( continuousMatch
    , auctionMatch
    , calcOpeningPrice
    ) where

import Data.List (foldl')

import Domain.ME
import Infra.Coverage


continuousMatch :: Order -> OrderBook -> Coverage (OrderBook, [Trade])
continuousMatch o ob = do
    let oq = oppositeSideQueue o ob
    (remo, oq', ts) <- _match o oq
    let ob'=  updateOppositeQueueInBook o oq' ob
    let ob'' = enqueue remo ob'
    case remo of
        Just remainder -> (ob'', ts) `covers` ("DF-D-ob-" ++ show (oid remainder))
        _ -> (ob'', ts) `covers` "DF-tau"
    -- (ob'', ts) `covers` "MNO"


auctionMatch :: MEState -> Coverage (OrderBook, [Trade])
auctionMatch s
    | Nothing <- op = (ob, []) `covers` "AM-no-opening-price" -- TODO: What should happened when there is no opening price?
    | Just p  <- op = _auctionMatchHelper [] ob p `covers` "AM-success"
    where
        op = calcOpeningPrice s
        ob = orderBook s

_auctionMatchHelper :: [Trade] -> OrderBook -> Price -> (OrderBook, [Trade])
_auctionMatchHelper ts ob op
    | not canMatch = (ob, ts)
    | bqty == sqty = _auctionMatchHelper (trade op bqty bo so : ts) ob { buyQueue = tail bq, sellQueue = tail sq } op
    | bqty  > sqty = _auctionMatchHelper (trade op sqty bo so : ts) ob { buyQueue = decQty bo sqty : tail bq, sellQueue = tail sq } op
    | bqty  < sqty = _auctionMatchHelper (trade op bqty bo so : ts) ob { buyQueue = tail bq, sellQueue = decQty so bqty : tail sq } op
    where
        bq = buyQueue ob
        sq = sellQueue ob
        bo = head bq
        so = head sq
        bqty = quantity bo
        sqty = quantity so
        canMatch = (price bo >= op) && (price so <= op)


calcOpeningPrice :: MEState -> OpeningPrice
calcOpeningPrice s
    | not $ isAuction s = Nothing
    | null sq || null bq = Nothing
    | sellBestPrice > buyBestPrice = Nothing
    | otherwise = Just bestPrice
    where
        ob = orderBook s
        sq = sellQueue ob
        bq = buyQueue ob
        sellBestPrice = price $ head sq
        buyBestPrice = price $ head bq
        refp = referencePrice s
        tick = tickSize s
        prices = [sellBestPrice, sellBestPrice + tick .. buyBestPrice]
        bestPrice =foldl' (\best p -> if _isBetterOpeningPrice ob refp p best then p else best) sellBestPrice prices


_isBetterOpeningPrice :: OrderBook -> Price -> Price -> Price -> Bool
_isBetterOpeningPrice ob refp p1 p2
    | tqp1 /= tqp2 = tqp1 > tqp2
    | ntqp1 /= ntqp2 = ntqp1 < ntqp2
    | otherwise = abs (p1 - refp) < abs (p2 - refp)
    where
        tqp1  = _canBeTradedQuantity ob p1
        tqp2  = _canBeTradedQuantity ob p2
        ntqp1 = _canNotBeTradedQuantity ob p1
        ntqp2 = _canNotBeTradedQuantity ob p2


_canBeTradedQuantity :: OrderBook -> Price -> Quantity
_canBeTradedQuantity ob p = min sellQueueCanBeTradedQty buyQueueCanBeTradedQty
    where
        sellQueueCanBeTradedQty = _canBeMatchedWithPriceQuantity (sellQueue ob) p
        buyQueueCanBeTradedQty = _canBeMatchedWithPriceQuantity (buyQueue ob) p


_canNotBeTradedQuantity :: OrderBook -> Price -> Quantity
_canNotBeTradedQuantity ob p = abs (sellQueueCanBeTradedQty - buyQueueCanBeTradedQty)
    where
        sellQueueCanBeTradedQty = _canBeMatchedWithPriceQuantity (sellQueue ob) p
        buyQueueCanBeTradedQty = _canBeMatchedWithPriceQuantity (buyQueue ob) p


_canBeMatchedWithPriceQuantity :: OrderQueue -> Price -> Quantity
_canBeMatchedWithPriceQuantity oq p = sum $ map quantity $ filter (`_canBeMatchedWithPrice` p) oq


_canBeMatchedWithPrice :: Order -> Price -> Bool
_canBeMatchedWithPrice o p
    | s == Buy  = op >= p
    | s == Sell = op <= p
    where
        s = side o
        op = price o


_canBeMatchedWithOppositeQueueHead :: Order -> Order -> Bool
_canBeMatchedWithOppositeQueueHead o h
    | s == Buy  = newp >= headp
    | s == Sell = newp <= headp
  where
    s = side o
    newp = price o
    headp = price h


_enqueueRemainder :: OrderQueue -> Order -> Coverage OrderQueue
_enqueueRemainder os o@LimitOrder {}
    | q == 0 = os `covers` ("ELR-1 DF-U-ob-" ++ show id)
    | otherwise = enqueueOrder o os `covers` ("ELR-2 DF-U-ob-" ++ show id ++ " DF-D-ob-" ++ show id)
  where
    q = quantity o
    id = oid o

_enqueueRemainder os o@IcebergOrder {}
    | q == 0 = os `covers` ("EIR-1 DF-U-ob-" ++ show id)
    | vq == 0 && q <= dq = enqueueOrder (setVisibleQty o q) os `covers` ("EIR-2 DF-U-ob-" ++ show id ++ " DF-D-ob-" ++ show id)
    | vq == 0 && q > dq = enqueueOrder (setVisibleQty o dq) os `covers` ("EIR-3 DF-U-ob-" ++ show id ++ " DF-D-ob-" ++ show id)
    | otherwise = enqueueOrder o os `covers` ("EIR-4 DF-U-ob-" ++ show id ++ " DF-D-ob-" ++ show id)
  where
    id = oid o
    q = quantity o
    vq = visibleQty o
    dq = disclosedQty o


_match :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
_match o [] = (Just o, [], []) `covers` "M-0"

_match o oq@(h:os)
    | not $ _canBeMatchedWithOppositeQueueHead o h = (Just o, oq, []) `covers` ("M-1 DF-U-ob-" ++ show qid )
    | newq < headq = (Nothing, (decQty h newq):os, [trade headp newq o h]) `covers` ("M-2 DF-U-ob-" ++ show qid ++ " DF-D-ob-" ++ show qid)
    | newq == headq = do
        newQueue <- _enqueueRemainder os $ decQty h newq
        (Nothing, newQueue, [trade headp newq o h]) `covers` "M-3"
    | newq > headq = do
        newQueue <- (_enqueueRemainder os $ decQty h headq)
        (o', oq', ts') <- _match (decQty o headq) newQueue
        (o', oq', (trade headp headq o h):ts') `covers` "M-4"
  where
    id = oid o
    qid = oid h
    newq = quantity o
    headp = price h
    headq = displayedQty h
