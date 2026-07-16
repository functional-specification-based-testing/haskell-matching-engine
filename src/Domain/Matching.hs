module Domain.Matching 
    ( continuousMatch
    , auctionMatch
    , calcOpeningPrice
    ) where

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


auctionMatch :: OrderBook -> Coverage (OrderBook, [Trade])
auctionMatch ob = (ob, []) `covers` "AM" -- TODO: Implement after finishing high level logics


calcOpeningPrice :: OrderBook -> OpeningPrice
calcOpeningPrice ob = Just 1 -- TODO: Implement after finishing high level logics


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
