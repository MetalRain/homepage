module Main where

import Prelude hiding (head, div, span)
import RawHTML(Element)
import EasyHTML as EH

description = "Software Engineer focused to Web services, interested in functional programming, information security and learning new stuff."

profileLinks = [ ("https://github.com/MetalRain", "GitHub")
               , ("https://fi.linkedin.com/in/otto-martikainen-70134335", "LinkedIn")
               , ("http://www.last.fm/user/MMetalRain", "Last.fm")
               ]

coolLinks = [ ("https://news.ycombinator.com/news","Hacker News")
            , ("https://medium.com", "Medium")
            , ("http://asofterworld.com", "A softer world")
            ]

linkItem :: (String,String) -> Element
linkItem (url, name) = EH.li [ (EH.attr "href" url) $ EH.a [ EH.text name ] ] 

linkList :: [(String,String)] -> Element
linkList links = EH.ul (map linkItem links)

buildDoc :: String -> String -> String
buildDoc style script = EH.html5 doc where
    doc     = (EH.attr "lang" "en") $ EH.html [docHead, docBody]
    docHead = EH.head [ EH.style [ EH.text style ] ]
    docBody = EH.body [ EH.h1 [ EH.text "MetalRain" ]
                   , (EH.attr "id" "content") $ EH.div content
                   , EH.script $ EH.textContent script
                   ]
    content = [ EH.h2 $ EH.textContent "About"
              , EH.p  $ EH.textContent description
              , EH.h2 $ EH.textContent "Links"
              , EH.h3 $ EH.textContent "Me"
              , linkList profileLinks
              , EH.h3 $ EH.textContent "Cool pages"
              , linkList coolLinks
              ]

main :: IO ()
main = do
  css <- readFile "data/main.css"
  js  <- readFile "data/main.js"
  putStrLn $ buildDoc css js
