module Decorators.CreditLimit (creditLimitProc) where

import qualified Data.Map        as Map

import           Domain.ME
-- import           Infra.Coverage
import           Infra.Decorator


creditSpentByBuyer :: BrokerID -> [Trade] -> Int
creditSpentByBuyer buyerId ts =
    sum $
    map valueTraded $
    filter (\t -> sellerBrId t /= buyerId) ts


totalWorthInQueue :: Side -> BrokerID -> OrderBook -> Int
totalWorthInQueue side bri ob =
    sum $
    map (\o -> price o * quantity o) $
    filter (\o -> brid o == bri) $
    queueBySide side ob


creditLimitCheckForArrivingOrder :: Order -> MEState -> [Trade] -> MEState -> Bool
creditLimitCheckForArrivingOrder o beforeTradeState ts afterTradeState = do
    bri `Map.member` credits && case side o of
        Buy  -> creditInfo beforeTradeState Map.! bri >= creditSpentByBuyer bri ts + totalWorthInQueue Buy bri afterTrade
        Sell -> True
  where
    bri = brid o
    credits = creditInfo beforeTradeState
    afterTrade = orderBook afterTradeState


updateCreditInfo :: [Trade] -> MEState -> MEState
updateCreditInfo ts s =
    foldl updateCreditByTrade s ts


updateCreditByTrade :: MEState -> Trade -> MEState
updateCreditByTrade s t =
    s''
  where
    s' = updateSellerCreditByTrade s t
    s'' = updateBuyerCreditByTrade s' t


updateBuyerCreditByTrade :: MEState -> Trade -> MEState
updateBuyerCreditByTrade state t =
    state {creditInfo = Map.insert bid newCredit ci}
  where
    bid = buyerBrId t
    ci = creditInfo state
    newCredit = ci Map.! bid - valueTraded t


updateSellerCreditByTrade :: MEState -> Trade -> MEState
updateSellerCreditByTrade state t =
    state {creditInfo = Map.insert sid newCredit ci}
  where
    sid = sellerBrId t
    ci = creditInfo state
    newCredit = ci Map.! sid + valueTraded t


creditLimitProc :: Decorator
creditLimitProc =
    decorateOnAccept creditLimitProcByType


creditLimitProcByType :: PartialDecorator
creditLimitProcByType rq@NewOrderRq {} s rs =
    creditLimitProcForArrivingOrder rq s rs

creditLimitProcByType rq@ReplaceOrderRq {} s rs =
    creditLimitProcForArrivingOrder rq s rs

creditLimitProcByType _ _ rs =rs


creditLimitProcForArrivingOrder :: PartialDecorator
creditLimitProcForArrivingOrder rq s rs = do
    let o = order rq
    let s' = state rs
    if creditLimitCheckForArrivingOrder o s (trades rs) s'
        then rs { state = updateCreditInfo (trades rs) s'} 
        else reject rq s 
