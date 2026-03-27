port module Main exposing (..)

import Browser
import Cranberry.Logic exposing (init, update, JSFlags)
import Cranberry.Model exposing (..)
import Cranberry.View exposing (view)

port viewport : (ViewportSize -> msg) -> Sub msg
port clipboardCopy : String -> Cmd msg

main : Program JSFlags Model Msg
main = Browser.element { init = init, update = update { clipboardCopy = clipboardCopy }, view = view, subscriptions = \_ -> viewport SubViewport }
