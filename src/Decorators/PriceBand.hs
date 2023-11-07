module Decorators.PriceBand (pricebandCheck) where

import           Domain.ME
import           Infra.Coverage
import           Infra.Decorator


pricebandCheck :: Decorator
pricebandCheck =
    decorateOnAccept "PBC" pricebandCheckByType


pricebandCheckByType :: PartialDecorator
pricebandCheckByType rq@NewOrderRq {} s rs = do
    pricebandCheckForArrivingOrder rq s rs

pricebandCheckByType rq@ReplaceOrderRq {} s rs = do
    pricebandCheckForArrivingOrder rq s rs

pricebandCheckByType _ _ rs =
    rs `covers` "PBC-P"


pricebandCheckForArrivingOrder :: PartialDecorator
pricebandCheckForArrivingOrder rq s rs = do
    let o = order rq
    let rp = referencePrice s
    let minPriceBandPortion = staticPriceBandLowerLimit s
    let maxPriceBandPortion = staticPriceBandUpperLimit s
    if pricebandPreCheck minPriceBandPortion maxPriceBandPortion rp o
        then rs `covers` "PBC1 DF-U-price_band"
        else reject rq s `covers` "PBC2 DF-U-price_band"


pricebandPreCheck :: Float -> Float -> Int -> Order -> Bool
pricebandPreCheck minPriceBandPortion maxPriceBandPortion referencePrice o =
    lowerPriceLimit <= p && p <= upperpriceLimit
  where
    p = price o
    upperpriceLimit = referencePrice + floor (fromIntegral referencePrice * maxPriceBandPortion)
    lowerPriceLimit = referencePrice - floor (fromIntegral referencePrice * minPriceBandPortion)
