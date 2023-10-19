module Parser
    ( genRequest
    , indexRequests
    ) where

import           Domain.ME


genRequest :: OrderID -> [String] -> Request
genRequest rqid (t:spec)
    | t == "NewOrderRq" = NewOrderRq $ genOrder rqid spec
    | t == "ReplaceOrderRq" = genReplaceOrderRq rqid spec
    | t == "CancelOrderRq" = genCancelOrderRq rqid spec
    | t == "SetCreditRq" = genSetCreditRq spec
    | t == "SetOwnershipRq" = genSetOwnershipRq spec
    | t == "SetReferencePriceRq" = genSetReferencePriceRq spec
    | t == "SetTotalSharesRq" = genSetTotalSharesRq spec
    | t == "SetStaticPriceBandLowerLimitRq" = genSetStaticPriceBandLowerLimitRq spec
    | t == "SetStaticPriceBandUpperLimitRq" = genSetStaticPriceBandUpperLimitRq spec
    | t == "SetOwnershipUpperLimitRq" = genSetOwnershipUpperLimitRq spec
    | t == "SetTickSizeRq" = genSetTickSizeRq spec
    | t == "SetLotSizeRq" = genSetLotSizeRq spec
    | otherwise = error $ "Invalid Request type " ++ t

genRequest _ [] = error "Empty Request"



indexRequests :: [String] -> [(Int, String)]
indexRequests =
    go 1 1
  where
    go i j (h:t) = if head (words h) `elem` ["NewOrderRq", "ReplaceOrderRq", "CancelOrderRq"]
        then (i, h) : go (i + 1) j t
        else (j, h) : go i (j + 1) t
    go _ _ _     = []


genSetCreditRq :: [String] -> Request
genSetCreditRq spec =
    SetCreditRq brokerID credit
  where
    brokerID = read $ spec !! 0 :: BrokerID
    credit = read $ spec !! 1 :: Int


genSetOwnershipRq :: [String] -> Request
genSetOwnershipRq spec =
    SetOwnershipRq shareholderID credit
  where
    shareholderID = read $ spec !! 0 :: ShareholderID
    credit = read $ spec !! 1 :: Int


genSetReferencePriceRq :: [String] -> Request
genSetReferencePriceRq spec =
    SetReferencePriceRq rp
  where
    rp = read $ head spec :: Price


genSetTotalSharesRq :: [String] -> Request
genSetTotalSharesRq spec =
    SetTotalSharesRq ts
  where
    ts = read $ head spec :: Quantity


genSetStaticPriceBandLowerLimitRq :: [String] -> Request
genSetStaticPriceBandLowerLimitRq spec =
    SetStaticPriceBandLowerLimitRq pb
  where
    pb = read $ head spec :: Float


genSetStaticPriceBandUpperLimitRq :: [String] -> Request
genSetStaticPriceBandUpperLimitRq spec =
    SetStaticPriceBandUpperLimitRq pb
  where
    pb = read $ head spec :: Float


genSetOwnershipUpperLimitRq :: [String] -> Request
genSetOwnershipUpperLimitRq spec =
    SetOwnershipUpperLimitRq ol
  where
    ol = read $ head spec :: Float


genSetTickSizeRq :: [String] -> Request
genSetTickSizeRq spec =
    SetTickSizeRq tick
  where
    tick = read $ head spec :: Price


genSetLotSizeRq :: [String] -> Request
genSetLotSizeRq spec =
    SetLotSizeRq lot
  where
    lot = read $ head spec :: Quantity


genOrder :: OrderID -> [String] -> Order
genOrder newoid spec =
    if isIceberge
        then icebergOrder newoid brokerId shareholderID price qty (if isBuy then Buy else Sell) (if hasMQ then Just minQty else Nothing) isFAK disclosedQty disclosedQty
        else limitOrder newoid brokerId shareholderID price qty (if isBuy then Buy else Sell) (if hasMQ then Just minQty else Nothing) isFAK
  where
        brokerId = read $ spec !! 0 :: BrokerID
        shareholderID = read $ spec !! 1 :: ShareholderID
        price = read $ spec !! 2 :: Price
        qty = read $ spec !! 3 :: Quantity
        isBuy = read $ spec !! 4 :: Bool
        minQty = read $ spec !! 5 :: Quantity
        hasMQ = minQty > 0
        isFAK = read $ spec !! 6 :: Bool
        disclosedQty = read $ spec !! 7 :: Quantity
        isIceberge = disclosedQty > 0


genCancelOrderRq :: OrderID -> [String] -> Request
genCancelOrderRq newoid spec =
    CancelOrderRq newoid oid side
  where
        oid = read $ spec !! 0 :: OrderID
        isBuy = read $ spec !! 1 :: Bool
        side = if isBuy then Buy else Sell


genReplaceOrderRq :: OrderID -> [String] -> Request
genReplaceOrderRq newoid spec =
    ReplaceOrderRq oldoid o
  where
        oldoid = read $ head spec :: OrderID
        o = genOrder newoid $ tail spec
