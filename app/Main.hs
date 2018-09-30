module Main where

import Prelude hiding (head, div, span)
import RawHTML(Element)
import EasyHTML as EH

aboutMe = [ "Software engineer and computer enthusiast. Interested in programming languages, history of computing and learning new stuff."
          , "Likes to cook without recipes and get immersed in virtual worlds."
          ]

profileLinks = [ ("https://github.com/MetalRain", "GitHub")
               , ("https://fi.linkedin.com/in/otto-martikainen", "LinkedIn")
               ]

linkItem :: (String,String) -> Element
linkItem (url, name) = EH.li [ (EH.attr "href" url) $ EH.a [ EH.text name ] ] 

linkList :: [(String,String)] -> Element
linkList links = EH.ul (map linkItem links)

buildDoc :: String -> String
buildDoc style = EH.html5 doc where
    doc     = (EH.attr "lang" "en") $ EH.html [docHead, docBody]
    docHead = EH.head [ (EH.attr "charset" "UTF-8") $ EH.meta []
                      , EH.style [ EH.text style ]
                      ]
    docBody = EH.body [ EH.attr "class" "background" $ EH.div []
                      , EH.attr "class" "content" $ EH.div [ header, content ]
                      ]

    header = EH.header [ title ]
    content = EH.section $ about ++ links

    title = EH.h1 $ EH.textContent "MetalRain"
    about  = [ EH.h2 $ EH.textContent "About" ] ++ (map (\s -> EH.p $ EH.textContent s) aboutMe)
    links = [ EH.h2 $ EH.textContent "Links", linkList profileLinks ]

main :: IO ()
main = do
  css <- readFile "data/main.css"
  putStrLn $ buildDoc css
