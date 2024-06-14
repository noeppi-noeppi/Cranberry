{-# LANGUAGE TemplateHaskell #-}

module Cranberry.TemplatePages where

import Data.String ( IsString(..) )
import Data.Text as T
import Data.ByteString (ByteString)
import Data.FileEmbed
import Cranberry.Types
import Text.Blaze.Html (Html, ToMarkup, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

s :: (IsString a) => String -> a
s = fromString

stylesheet :: MessageContent T.Text
stylesheet = MessageContent "text/css" $ fromString "*{font-family:'Lato Bold','Noto Sans','Open Sans','OpenSans','Roboto',sans-serif;}"

skeleton :: (ToMarkup a) => a -> Html -> Html -> Html
skeleton title head content = H.docTypeHtml $ do
  H.head $ do
    H.title (toHtml title)
    H.meta ! A.charset (s "utf-8")
    head
  H.body content

indexPage :: Html
indexPage = skeleton "Cranberry" (H.style ! A.type_ (s "text/css") $ s "body{padding:0;margin:0;}") $ do
  H.pre ! A.id (s "elm") $ mempty
  H.script ! A.src (s "/_/index.js") ! A.type_ (s "application/javascript") $ mempty
  H.script ! A.type_ (s "application/javascript") $ do
    s "var app = Elm.Main.init({node:document.getElementById('elm')});"

errorPage :: String -> Html
errorPage message = skeleton "Error" (H.link ! A.rel (s "stylesheet") ! A.href (s "/_/style.css")) $ do
    H.h1 (toHtml $ "Error: " ++ message)

indexScript :: MessageContent ByteString
indexScript = MessageContent "application/javascript" $(embedFile "web/index.js")
