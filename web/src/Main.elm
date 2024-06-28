port module Main exposing (..)

import Browser
import Cranberry.Logic exposing (init, update, JSFlags)
import Cranberry.Model exposing (..)
import Cranberry.View exposing (view)

port viewport : (ViewportSize -> msg) -> Sub msg

main : Program JSFlags Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> viewport SubViewport }
