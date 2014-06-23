module Upcast.Interpolate.Parse where

import Upcast.Interpolate.Util

data Node = Literal String | Expression String

parseNodes :: String -> [Node]
parseNodes = go ""
  where
    go acc input = case input of
      ""  -> [(lit . reverse) acc]
      '#':'{':xs -> case span (/= '}') xs of
        (ys, _:zs) -> (lit . reverse) acc : Expression ys : go "" zs
        (_, "") -> [(Literal . unescape) (reverse acc ++ input)]
      x:xs -> go (x:acc) xs

    lit :: String -> Node
    lit acc = (Literal . unescape) acc
