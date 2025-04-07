module Domain.Parser
    ( assignId
    , indexRequests
    ) where

import           Domain.ME

assignId :: OrderID -> Request -> Request
assignId rqid rq@(NewOrderRq order) = NewOrderRq $ setId rqid order
assignId rqid rq@CancelOrderRq{} = rq {rqId = rqid}
assignId rqid rq@(ReplaceOrderRq oldid order) = ReplaceOrderRq oldid $ setId rqid order
assignId rqid rq = rq

isOrderRequest :: Request -> Bool
isOrderRequest rq = case rq of
    NewOrderRq {} -> True
    ReplaceOrderRq {} -> True
    CancelOrderRq {} -> True
    _ -> False

indexRequests :: [Request] -> [(Int, Request)]
indexRequests =
    go 1 1
  where
    go i j (h:t) = if isOrderRequest h
        then (i, h) : go (i + 1) j t
        else (j, h) : go i (j + 1) t
    go _ _ _     = []
