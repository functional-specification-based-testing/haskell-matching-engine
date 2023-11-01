module Decorators.MinQuantity (minQuantityCheck) where

import           Domain.ME
-- import           Infra.Coverage
import           Infra.Decorator


minQuantityCheck :: Decorator
minQuantityCheck =
    decorateOnAccept minQuantityCheckByType


minQuantityCheckByType :: PartialDecorator
minQuantityCheckByType !rq@(NewOrderRq o) !s !rs =
    case minQty o of
        Nothing -> rs 
        Just mq -> if sum (Prelude.map quantityTraded $! trades rs) >= mq
            then rs 
            else (reject rq s) { status = Eliminated } 

minQuantityCheckByType _ _ rs =rs 
