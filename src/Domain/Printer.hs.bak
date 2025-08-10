module Domain.Printer
    ( fRequest
    , fResponse
    ) where

import qualified Data.Map    as Map
import           Text.Printf

import           Domain.ME


fRequest :: Request -> String
fRequest (NewOrderRq o) =
    printf "NewOrderRq\t\t%s\n" $ fOrder o

fRequest (CancelOrderRq rqid oid side) =
    printf "CancelOrderRq\t%d\t\t%d\t\t\t\t\t%s\n" oid rqid $ fSide side

fRequest (ReplaceOrderRq oldoid o) =
    printf "ReplaceOrderRq\t%d\t%s\n" oldoid $ fOrder o

fRequest (SetCreditRq b c) =
    printf "SetCreditRq\t%d\t%d\n" b c

fRequest (SetOwnershipRq sh i) =
    printf "SetOwnershipRq\t%d\t%d\n" sh i

fRequest (SetReferencePriceRq rp) =
    printf "SetReferencePriceRq\t%d\n" rp

fRequest (SetTotalSharesRq ts) =
    printf "SetTotalSharesRq\t%d\n" ts

fRequest (SetStaticPriceBandLowerLimitRq pb) =
    printf "SetStaticPriceBandLowerLimitRq\t%f\n" pb

fRequest (SetStaticPriceBandUpperLimitRq pb) =
    printf "SetStaticPriceBandUpperLimitRq\t%f\n" pb

fRequest (SetOwnershipUpperLimitRq ol) =
    printf "SetOwnershipUpperLimitRq\t%f\n" ol

fRequest (SetTickSizeRq tick) =
    printf "SetTickSizeRq\t%d\n" tick

fRequest (SetLotSizeRq lot) =
    printf "SetLotSizeRq\t%d\n" lot


fResponse :: Response -> String
fResponse (NewOrderRs s ts statesnapshot) =
    printf "NewOrderRs\t%s\n%s%s" (show s) (fTrades ts) (fState statesnapshot)

fResponse (CancelOrderRs s _ statesnapshot) =
    printf "CancelOrderRs\t%s\n%s" (show s) (fState statesnapshot)

fResponse (ReplaceOrderRs s _ ts statesnapshot) =
    printf "ReplaceOrderRs\t%s\n%s%s" (show s) (fTrades ts) (fState statesnapshot)

fResponse (SetCreditRs s _) =
    printf "SetCreditRs\t%s\n" (show s)

fResponse (SetOwnershipRs s _) =
    printf "SetOwnershipRs\t%s\n" (show s)

fResponse (SetReferencePriceRs s _) =
    printf "SetReferencePriceRs\t%s\n" (show s)

fResponse (SetTotalSharesRs s _) =
    printf "SetTotalSharesRs\t%s\n" (show s)

fResponse (SetStaticPriceBandLowerLimitRs s _) =
    printf "SetStaticPriceBandLowerLimitRs\t%s\n" (show s)

fResponse (SetStaticPriceBandUpperLimitRs s _) =
    printf "SetStaticPriceBandUpperLimitRs\t%s\n" (show s)

fResponse (SetOwnershipUpperLimitRs s _) =
    printf "SetOwnershipUpperLimitRs\t%s\n" (show s)

fResponse (SetTickSizeRs s _) =
    printf "SetTickSizeRs\t%s\n" (show s)

fResponse (SetLotSizeRs s _) =
    printf "SetLotSizeRs\t%s\n" (show s)


fState :: MEState -> String
fState state =
    printf "%s%s%s%s%s%s%s"
    (fOrderBook $ orderBook state)
    (fCreditInfo $ creditInfo state)
    (fOwnershipInfo $ ownershipInfo state)
    (fPriceBands (referencePrice state) (staticPriceBandLowerLimit state) (staticPriceBandUpperLimit state))
    (fOwnershipLimits (totalShares state) (ownershipUpperLimit state))
    (fTickSize $ tickSize state)
    (fLotSize $ lotSize state)


fMap :: Show a => Show b => String -> Map.Map a b -> String
fMap prefix m = concatMap (\(i, j) -> printf "\t%s\t%s\t%s\n" prefix (show i) (show j)) $ Map.toList m


fSide :: Side -> String
fSide Sell = "SELL"
fSide Buy  = "BUY "


fFAK :: Bool -> String
fFAK b = if b then "FAK" else "---"


fOptional :: Maybe Int -> Int
fOptional Nothing  = 0
fOptional (Just n) = n


fOrder :: Order -> String
fOrder (LimitOrder i bi shi p q s mq fak) =
    printf "Limit\t%d\t%d\t%d\t%d\t%d\t%s\t%d\t%s\t0" i bi shi p q (fSide s) (fOptional mq) (fFAK fak)

fOrder (IcebergOrder i bi shi p q s mq fak dq ps) =
    printf "Iceberg\t%d\t%d\t%d\t%d\t%d\t%s\t%d\t%s\t%d" i bi shi p q (fSide s) (fOptional mq) (fFAK fak) ps


fTrade :: Trade -> String
fTrade (Trade p q bid sid _ _ _ _) =
    printf "\tTrade\t%d\t%d\t%d\t%d\n" p q bid sid


fTrades :: [Trade] -> String
fTrades ts = foldl (++) (printf "\tTrades\t%d\n" $ length ts) $ map fTrade ts


fOrderBook :: OrderBook -> String
fOrderBook (OrderBook bq sq) = foldl (++) (printf "\tOrders\t%d\n" $ length bq + length sq) $ map (printf "\tOrder\t%s\n" . fOrder) $ bq ++ sq


fCreditInfo :: CreditInfo -> String
fCreditInfo cs = printf "\tCredits\t%d\n%s" (length cs) (fMap "Credit" cs)


fOwnershipInfo :: OwnershipInfo -> String
fOwnershipInfo os = printf "\tOwnerships\t%d\n%s" (length os) (fMap "Ownership" os)


fPriceBands :: Price -> Float -> Float -> String
fPriceBands = printf "\tReferencePrice\t%d\n\tStaticPriceBandLowerLimit\t%f\n\tStaticPriceBandUpperLimit\t%f\n"


fOwnershipLimits :: Quantity -> Float -> String
fOwnershipLimits = printf "\tTotalShares\t%d\n\tOwnershipUpperLimit\t%f\n"


fTickSize :: Price -> String
fTickSize = printf "\tTickSize\t%d\n"


fLotSize :: Quantity -> String
fLotSize = printf "\tLotSize\t%d\n"
