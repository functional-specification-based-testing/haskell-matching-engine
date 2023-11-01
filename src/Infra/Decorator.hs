module Infra.Decorator
    ( Handler
    , Decorator
    , PartialDecorator
    , decorateOnAccept
    ) where

import           Domain.ME
-- import           Infra.Coverage

type Handler = Request -> MEState ->  Response
type Decorator = Handler -> Handler
-- type Decorator = Request -> MEState ->  Response -> Request -> MEState ->  Response
type PartialDecorator = Request -> MEState -> Response ->  Response


decorateOnAccept :: PartialDecorator -> Handler -> Request -> MEState ->  Response
decorateOnAccept !decorateByType !handler !rq !s = do
    let !rs = handler rq s
    case status rs of
        Accepted   -> decorateByType rq s rs
        Eliminated -> rs
        Rejected   -> rs 
