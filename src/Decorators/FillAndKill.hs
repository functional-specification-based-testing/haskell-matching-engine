module Decorators.FillAndKill (fillAndKillProc) where

import           Domain.ME
-- import           Infra.Coverage
import           Infra.Decorator


removeOrderFromState :: Order -> MEState -> MEState
removeOrderFromState !o !s =
  s { orderBook = removeOrderFromOrderBook o (orderBook s) }


fillAndKillProc :: Decorator
fillAndKillProc =
    decorateOnAccept  fillAndKillProcByType


fillAndKillProcByType :: PartialDecorator
fillAndKillProcByType  !(NewOrderRq o) _ !rs = do
    let !s' = state rs
    if fillAndKill o
        then rs { state = removeOrderFromState o s', status = accepted_status }
        else rs 
  where
    !accepted_status = if sum (Prelude.map quantityTraded $! trades rs) > 0
        then status rs
        else Eliminated

fillAndKillProcByType  (ReplaceOrderRq _ o) _ rs = do
    let !s' = state rs
    if fillAndKill o
        then rs { state = removeOrderFromState o s', status = accepted_status } 
        else rs 
  where
    !accepted_status = if sum (Prelude.map quantityTraded $! trades rs) > 0
        then status rs
        else Eliminated

fillAndKillProcByType _ _ rs = rs
