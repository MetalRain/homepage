module EasyHTML 
    ( html5
    , el
    , attr
    , text
    , html
    , body
    , head
    , script
    , style
    , link
    , div
    , ul
    , ol
    , li
    , p
    , a
    , span
    , input
    , img
    ) where

import Prelude hiding (head, div, span)
import Data.Monoid
import RawHTML

html5 :: Element -> String
html5 root = (docType "html") <> (show root) 

text :: String -> Element
text str = TextNode (Value str)

el :: String -> [Element] -> Element
el tag = mkElement tag []

attr :: String -> String -> Element -> Element
attr name value (Element tag attrs children) = Element tag ((mkAttr name value) : attrs) children

html :: [Element] -> Element
html = el "html"

head :: [Element] -> Element
head = el "head"

body :: [Element] -> Element
body = el "body"

script :: [Element] -> Element
script = el "script"

link :: [Element] -> Element
link = el "link"

style :: [Element] -> Element
style = el "style"

div :: [Element] -> Element
div = el "div"

ul :: [Element] -> Element
ul = el "ul"

ol :: [Element] -> Element
ol = el "ol"

li :: [Element] -> Element
li = el "li"

p :: [Element] -> Element
p = el "p"

a :: [Element] -> Element
a = el "a"

span :: [Element] -> Element
span = el "span"

input :: [Element] -> Element
input = el "input"

img :: [Element] -> Element
img = el "img"
