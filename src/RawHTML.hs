module RawHTML
    ( Name(..)
    , Value(..)
    , Attr(..)
    , Element(..)
    , mkAttr
    , mkElement
    , docType
    ) where

import Data.Monoid
import Data.List

newtype Name = Name String
newtype Value = Value String
data Attr = Attr Name Value
data Element = Element Name [Attr] [Element] 
             | TextNode Value

wrapWith :: String -> String -> String -> String
wrapWith start end value = start <> value <> end 

quotes :: String -> String
quotes = wrapWith "\"" "\""

showQuoted :: (Show a) => a -> String
showQuoted = quotes . show

startTag :: String -> String
startTag = wrapWith "<" ">"

closeTag :: String -> String
closeTag = wrapWith "</" ">"

docType :: String -> String
docType = wrapWith "<!DOCTYPE " ">\n"

showMany :: (Show a) => String -> [a] -> String
showMany sep xs = intercalate sep (map show xs)

instance Show Element where
  show (TextNode value) = (show value)
  show (Element (Name name) attrs children) = start <> content <> close where
    start   = startTag $ intercalate " " (name : map show attrs)
    content = showMany "" children
    close   = closeTag name

instance Show Name where
  show (Name name) = name

instance Show Value where
  show (Value value) = value

instance Show Attr where
  show (Attr name value) = show name <> "=" <> (showQuoted value)

mkAttr :: String -> String -> Attr
mkAttr k v = Attr (Name k) (Value v)

mkElement :: String -> [Attr] -> [Element] -> Element
mkElement tag = Element (Name tag)
