module Decorators.Validation
    ( validateOrder
    , postponedCheckOnReplace
    ) where

import           Domain.ME
import           Infra.Coverage
import           Infra.Decorator


validateOrder :: Decorator
validateOrder hdlr =
    decorateOnAccept "VAL-PR" validatePriceWrapper $
    decorateOnAccept "VAL-QTY" validateQtyWrapper $
    decorateOnAccept "VAL-AttrConsistency" validateAttrConsistencyWrapper $
    decorateOnAccept "VAL-Replace" validateOnReplaceWrapper $
    hdlr


validatePriceWrapper :: PartialDecorator
validatePriceWrapper rq@NewOrderRq{} s rs =
    validatePrice rq s rs

validatePriceWrapper rq@ReplaceOrderRq{} s rs =
    validatePrice rq s rs

validatePriceWrapper _ _ rs =
    rs `covers` "VAL-PR-P"


validatePrice :: PartialDecorator
validatePrice rq s rs
    | p <= 0 = reject rq s `covers` "VAL-PR-non-posetive-price"
    | p `rem` tick /= 0 = reject rq s `covers` "VAL-PR-tick-violation"
    | otherwise = rs `covers` "VAL-PR-passed"
  where
    o = order rq
    p = price o
    tick = tickSize s


validateQtyWrapper :: PartialDecorator
validateQtyWrapper rq@NewOrderRq{} s rs =
    validateQty rq s rs

validateQtyWrapper rq@ReplaceOrderRq{} s rs =
    validateQty rq s rs

validateQtyWrapper _ _ rs =
    rs `covers` "VAL-QTY-P"


validateQty :: PartialDecorator
validateQty rq s rs
    | q <= 0 = reject rq s `covers` "VAL-QTY-non-posetive-qty"
    | q `rem` lot /= 0 = reject rq s `covers` "VAL-QTY-lot-violation"
    | not $ validateIcebergQty o = reject rq s `covers` "VAL-QTY-lot-invalid-disclosed-qty"
    | not $ validateMinQty o = reject rq s `covers` "VAL-QTY-lot-invalid-min-qty"
    | otherwise = rs `covers` "VAL-QTY-passed"
  where
    o = order rq
    q = quantity o
    lot = lotSize s


validateIcebergQty :: Order -> Bool
validateIcebergQty LimitOrder {} =
    True

validateIcebergQty order@IcebergOrder {} =
    d > 0 && d <= q
  where
    q = quantity order
    d = disclosedQty order


validateMinQty :: Order -> Bool
validateMinQty order =
    case m of
        Nothing -> True
        Just mq -> mq > 0 && mq <= q
  where
    m = minQty order
    q = quantity order


validateAttrConsistencyWrapper :: PartialDecorator
validateAttrConsistencyWrapper rq@NewOrderRq{} s rs =
    validateAttrConsistency rq s rs

validateAttrConsistencyWrapper rq@ReplaceOrderRq{} s rs =
    validateAttrConsistency rq s rs

validateAttrConsistencyWrapper _ _ rs =
    rs `covers` "VAL-AttrConsistency-P"


validateAttrConsistency :: PartialDecorator
validateAttrConsistency rq s rs
    | not $ validateFakWithIceberg o = reject rq s `covers` "VAL-AttrConsistency-iceberg-fak"
    | otherwise = rs `covers` "VAL-AttrConsistency-passed"
  where
    o = order rq


validateFakWithIceberg :: Order -> Bool
validateFakWithIceberg LimitOrder {} =
    True

validateFakWithIceberg order@IcebergOrder {} =
    not fak
  where
    fak = fillAndKill order


validateOnReplaceWrapper :: PartialDecorator
validateOnReplaceWrapper rq@ReplaceOrderRq{} s rs =
    validateOnReplace rq s rs

validateOnReplaceWrapper _ _ rs =
    rs `covers` "VAL-Replace-P"


validateOnReplace :: PartialDecorator
validateOnReplace rq s rs
    | not $ allowMinQty o = reject rq s `covers` "VAL-Replace-with-min-qty"
    | otherwise = rs `covers` "VAL-Replace-passed"
  where
    o = order rq


allowMinQty :: Order -> Bool
allowMinQty order =
    case m of
        Nothing -> True
        Just _  -> False
  where
    m = minQty order


postponedCheckOnReplace :: Order -> Order -> Bool
postponedCheckOnReplace oldOrder newOrderNotAdjusted =
    newShareholder == oldShareholder &&
    newBroker == oldBroker &&
    newSide == oldSide
  where
    newShareholder = shid newOrderNotAdjusted
    oldShareholder = shid oldOrder
    newBroker = brid newOrderNotAdjusted
    oldBroker = brid oldOrder
    newSide = side newOrderNotAdjusted
    oldSide = side oldOrder
