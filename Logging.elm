module Logging where

type Logger a = (a, [String])

pure : a -> Logger a
pure a = (a, [])

andThen : Logger a -> (a -> Logger b) -> Logger b
andThen (a, ss) f =
    let (b, ss') = f a
    in  (b, ss' ++ ss)

and : Logger a -> Logger b -> Logger b
and (_, ss) (b, ss') = (b, ss' ++ ss)

get : Logger a -> a
get (a, _) = a

set : Logger a -> a -> Logger a
set (a, ss) a' = (a', ss)
