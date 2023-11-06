module Decorators.Validation
    ( validateOrder
    , postponedCheckOnReplace
    ) where

import          Domain.ME
-- import           Infra.Coverage
import          Infra.Decorator
import          Control.DeepSeq


validateOrder :: Decorator
validateOrder hdlr =
    decorateOnAccept validatePriceWrapper `deepseq`
    decorateOnAccept validateQtyWrapper `deepseq`
    decorateOnAccept validateAttrConsistencyWrapper `deepseq`
    decorateOnAccept validateOnReplaceWrapper `deepseq`
    hdlr


validatePriceWrapper :: PartialDecorator
validatePriceWrapper !rq@NewOrderRq{} !s !rs = validatePrice rq s rs

validatePriceWrapper !rq@ReplaceOrderRq{} !s !rs =
    validatePrice rq s rs

validatePriceWrapper _ _ !rs = rs


validatePrice :: PartialDecorator
validatePrice !rq !s !rs
    | p <= 0 = reject rq s 
    | p `rem` tick /= 0 = reject rq s 
    | otherwise = rs
  where
    !o = order rq
    !p = price o
    !tick = tickSize s


validateQtyWrapper :: PartialDecorator
validateQtyWrapper !rq@NewOrderRq{} !s !rs =
    validateQty rq s rs

validateQtyWrapper !rq@ReplaceOrderRq{} !s !rs =
    validateQty rq s rs

validateQtyWrapper _ _ !rs = rs


validateQty :: PartialDecorator
validateQty !rq !s !rs
    | q <= 0 = reject rq s 
    | q `rem` lot /= 0 = reject rq s 
    | not `deepseq` validateIcebergQty o = reject rq s 
    | not `deepseq` validateMinQty o = reject rq s 
    | otherwise = rs
  where
    !o = order rq
    !q = quantity o
    !lot = lotSize s


validateIcebergQty :: Order -> Bool
validateIcebergQty !LimitOrder {} =
    True

validateIcebergQty !order@IcebergOrder {} =
    d > 0 && d <= q
  where
    !q = quantity order
    !d = disclosedQty order


validateMinQty :: Order -> Bool
validateMinQty !order =
    case m of
        Nothing -> True
        Just mq -> mq > 0 && mq <= q
  where
    !m = minQty order
    !q = quantity order


validateAttrConsistencyWrapper :: PartialDecorator
validateAttrConsistencyWrapper !rq@NewOrderRq{} !s !rs =
    validateAttrConsistency rq s rs

validateAttrConsistencyWrapper !rq@ReplaceOrderRq{} !s !rs =
    validateAttrConsistency rq s rs

validateAttrConsistencyWrapper _ _ !rs =
    rs


validateAttrConsistency :: PartialDecorator
validateAttrConsistency !rq !s !rs
    | not `deepseq` validateFakWithIceberg o = reject rq s
    | not `deepseq` validateFakWithMinQty o = reject rq s 
    | otherwise = rs 
  where
    !o = order rq


validateFakWithIceberg :: Order -> Bool
validateFakWithIceberg !LimitOrder {} =
    True

validateFakWithIceberg !order@IcebergOrder {} =
    not fak
  where
    !fak = fillAndKill order


validateFakWithMinQty :: Order -> Bool
validateFakWithMinQty !order =
    case m of
        Nothing -> True
        Just _  -> not fak
  where
    !fak = fillAndKill order
    !m = minQty order


validateOnReplaceWrapper :: PartialDecorator
validateOnReplaceWrapper !rq@ReplaceOrderRq{} !s !rs =
    validateOnReplace rq s rs

validateOnReplaceWrapper _ _ !rs =
    rs 


validateOnReplace :: PartialDecorator
validateOnReplace !rq !s !rs
    | not `deepseq` allowMinQty o = reject rq s 
    | otherwise = rs
  where
    !o = order rq


allowMinQty :: Order -> Bool
allowMinQty !order =
    case m of
        Nothing -> True
        Just _  -> False
  where
    !m = minQty order


postponedCheckOnReplace :: Order -> Order -> Bool
postponedCheckOnReplace !oldOrder !newOrderNotAdjusted =
    newShareholder == oldShareholder &&
    newBroker == oldBroker &&
    newSide == oldSide
  where
    !newShareholder = shid newOrderNotAdjusted
    !oldShareholder = shid oldOrder
    !newBroker = brid newOrderNotAdjusted
    !oldBroker = brid oldOrder
    !newSide = side newOrderNotAdjusted
    !oldSide = side oldOrder
