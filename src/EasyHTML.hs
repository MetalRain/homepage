module EasyHTML 
    ( html5
    , el
    , attr
    , text
    , textContent
    , html
    , body
    , head
    , script
    , style
    , link
    , meta
    , div
    , ul
    , ol
    , li
    , p
    , a
    , span
    , input
    , img
    , h1
    , h2
    , h3
    , section
    , header
    ) where

import Prelude hiding (head, div, span)
import Data.Monoid
import RawHTML

html5 :: Element -> String
html5 root = (docType "html") <> (show root) 

text :: String -> Element
text str = TextNode (Value str)

textContent :: String -> [Element]
textContent str = [ text str ]

el :: String -> [Element] -> Element
el tag = mkElement tag []

voidEl :: String -> [Element] -> Element
voidEl tag = mkVoidElement tag []

attr :: String -> String -> Element -> Element
attr name value (Element elType tag attrs children) = Element elType tag ((mkAttr name value) : attrs) children

html :: [Element] -> Element
html = el "html"

head :: [Element] -> Element
head = el "head"

body :: [Element] -> Element
body = el "body"

script :: [Element] -> Element
script = el "script"

link :: [Element] -> Element
link = voidEl "link"

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
input = voidEl "input"

img :: [Element] -> Element
img = voidEl "img"

h1 :: [Element] -> Element
h1 = el "h1"

h2 :: [Element] -> Element
h2 = el "h2"

h3 :: [Element] -> Element
h3 = el "h3"

meta :: [Element] -> Element
meta = voidEl "meta"

section :: [Element] -> Element
section = el "section"

header :: [Element] -> Element
header = el "header"