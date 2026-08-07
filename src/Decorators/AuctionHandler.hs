module Decorators.AuctionHandler (auctionHandlerDecorator) where

import           Domain.ME
import           Domain.Matching
import           Infra.Decorator


auctionHandlerDecorator :: Decorator
auctionHandlerDecorator =
    decorateOnAccept "AHD-" auctionHandlerDecoratorOnAccept


auctionHandlerDecoratorOnAccept :: PartialDecorator
auctionHandlerDecoratorOnAccept _ s _ = do
    (ob', ts) <- auctionMatch $ orderBook s
    return (ChangeMatchingTypeRs Accepted ts s { orderBook = ob'})
