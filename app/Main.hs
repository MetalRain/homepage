module Main where

import Prelude hiding (head, div, span)
import RawHTML
import EasyHTML
import Data.Monoid

explode :: Int -> Element
explode x = (attr "class" (rowClass x)) $ li [ text $  msg x ]

msg :: Int -> String
msg x = "Bomb will explode in " <> (show x) <> " seconds.."

rowClass :: Int -> String
rowClass x = ["odd","even"]  !! (x `mod` 2)

main :: IO ()
main = putStrLn $ html5 doc where
  doc     = (attr "lang" "en") $ html [head [], content]
  content = body [ el "h1" [text "Welcome"], ul rows ]
  rows    = map explode [10,9..0]

