module Cranberry.TemplatePages (indexPage, errorPage, indexScript, stylesheet) where

import Data.String ( IsString(..) )
import Data.ByteString (ByteString)
import Data.FileEmbed
import Cranberry.Types
import Text.Blaze.Html (Html, ToMarkup, toHtml, (!))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

stylesheet :: MessageContent T.Text
stylesheet = MessageContent "text/css" $ fromString "*{font-family:'Lato Bold','Noto Sans','Open Sans','OpenSans','Roboto',sans-serif;}"

skeleton :: String -> Html -> Html -> Html
skeleton title head content = H.docTypeHtml $ do
  H.head $ do
    H.title (toHtml title)
    H.meta ! A.charset ("utf-8")
    head
  H.body content

indexStyle :: Html
indexStyle = do
  H.meta ! A.name "viewport" ! A.content "width=device-width,height=device-height,interactive-widget=resizes-visual"
  H.style ! A.type_ "text/css" $ "body{padding:0;margin:0;}"

indexPage :: Bool -> Html
indexPage oidcReturn = skeleton "Cranberry" indexStyle $ do
  H.pre ! A.id "elm" $ mempty
  H.script ! A.src "/_/index.js" ! A.type_ "application/javascript" $ mempty
  H.script ! A.type_ "application/javascript" $ do
    toHtml $ T.concat ["var oidc=", if oidcReturn then "true" else "false", ";", TE.decodeUtf8Lenient $(embedFile "web/src/Main.js")]

errorPage :: String -> Html
errorPage message = skeleton "Error" (H.link ! A.rel "stylesheet" ! A.href "/_/style.css") $ do
    H.h1 (toHtml $ "Error: " ++ message)

indexScript :: MessageContent ByteString
indexScript = MessageContent "application/javascript" $(embedFile "web/index.js")
