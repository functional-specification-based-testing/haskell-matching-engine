module Decorators.CreditLimit (creditLimitProc) where

import qualified Data.Map        as Map

import           Domain.ME
import           Infra.Coverage
import           Infra.Decorator
import           Control.Monad


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


creditLimitCheckForArrivingOrder :: Order -> MEState -> [Trade] -> MEState -> Coverage Bool
creditLimitCheckForArrivingOrder o beforeTradeState ts afterTradeState 
    | s == Buy = do 
        if  bri `Map.member` credits 
            then  (creditInfo beforeTradeState Map.! bri >= creditSpentByBuyer bri ts + totalWorthInQueue Buy bri afterTrade) `covers` "DF-U-credit"
            else  False `covers` "DF-tau"
    | s == Sell = do 
        if  bri `Map.member` credits
            then True `covers` "DF-tau"
            else False `covers` "DF-tau"
    where
        s = side o
        bri = brid o
        credits = creditInfo beforeTradeState
        afterTrade = orderBook afterTradeState

updateCreditInfo :: [Trade] -> MEState -> Coverage MEState
updateCreditInfo ts s =
    foldl updateCreditByTrade s ts `covers` "DF-U-credit DF-D-credit"


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
    decorateOnAccept "CLP" creditLimitProcByType


creditLimitProcByType :: PartialDecorator
creditLimitProcByType rq@NewOrderRq {} s rs =
    creditLimitProcForArrivingOrder rq s rs

creditLimitProcByType rq@ReplaceOrderRq {} s rs =
    creditLimitProcForArrivingOrder rq s rs

creditLimitProcByType _ _ rs =
    rs `covers` "CLP-P"


creditLimitProcForArrivingOrder :: PartialDecorator
creditLimitProcForArrivingOrder rq s rs = do
    let o = order rq
    let s' = state rs
    result <- creditLimitCheckForArrivingOrder o s (trades rs) s'
    if result
        then  do
            newState <-  updateCreditInfo (trades rs) s'
            rs { state = newState} `covers` "CLP1"
        else reject rq s `covers` "CLP2"
