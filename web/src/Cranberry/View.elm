module Cranberry.View exposing (..)

import Cranberry.Model exposing (..)
import Color
import Html exposing (Html)
import Element exposing (..)
import Widget as W
import Widget.Material as M
import Widget.Icon
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as I
import Dict

color : Color.Color -> Element.Color
color c = case Color.toRgba c of
  {red, green, blue, alpha} -> rgba red green blue alpha

icon : I.Icon -> Widget.Icon.Icon msg
icon ic = Widget.Icon.elmFeather I.toHtml ic

doLayout : M.Palette -> Element msg -> Element msg -> Maybe (W.Modal msg) -> Html msg
doLayout palette root notifs modal =
  let modalAttrs = (modal |> Maybe.map (\m -> W.singleModal [m]) |> Maybe.withDefault [])
  in root |> layout ([Background.color (color palette.background), Font.color (color palette.on.background)] ++ modalAttrs ++ [inFront notifs])

view : Model -> Html Msg
view model = case model of
  Undecided -> el [centerX, centerY] (W.circularProgressIndicator (M.progressIndicator M.defaultPalette) Nothing) |> layout []
  Failed -> el [centerX, centerY] (text "Something went wrong.") |> layout []
  Model content -> modalView content (if content.display.darkMode then M.darkPalette else M.defaultPalette)

modalView : ContentModel -> M.Palette -> Html Msg
modalView model palette = case model.auth of
  Anonymous form -> if model.display.showLoginDialog
    then doLayout palette (pageView model palette) (notifications model palette) (Just (loginDialog form palette))
    else doLayout palette (pageView model palette) (notifications model palette) Nothing
  _ -> doLayout palette (pageView model palette) (notifications model palette) Nothing

notifications : ContentModel -> M.Palette -> Element Msg
notifications model palette = model.notifications |> notifList 0 palette |> column [padding 3, spacing 3, alignRight, alignBottom] 

notifList : Int -> M.Palette -> List Notification -> List (Element Msg)
notifList idx palette list = case list of
  (Info msg) :: xs -> notifListItem msg idx palette.secondary palette.on.secondary :: notifList (idx + 1) palette xs
  (Failure msg) :: xs -> notifListItem msg idx palette.error palette.on.error :: notifList (idx + 1) palette xs
  [] -> []

notifListItem : String -> Int -> Color.Color -> Color.Color -> Element Msg
notifListItem msg idx bg fg = row [
  width (minimum 300 shrink), alignRight, padding 10, spacing 3, Border.rounded 5,
  Background.color (color bg), Font.color (color fg)] [
     el [alignLeft, padding 0] (text msg),
     Input.button [alignRight, padding 0] {onPress = Just (MsgDiscardNotification idx), label = I.toHtml [] I.x |> html}]

pageView : ContentModel -> M.Palette -> Element Msg
pageView model palette = column [ width fill, height fill ] [
  el [width fill] (W.menuBar (M.menuBar palette) {
    title = (case model.auth of
      LoggedIn name _ -> text ("Cranberry - " ++ name)
      Anonymous _ -> text "Cranberry"),
    deviceClass = Desktop,
    openLeftSheet = Nothing,
    openRightSheet = Nothing,
    openTopSheet = Nothing,
    search = Nothing,
    primaryActions = topBarActions model}),
  case model.role of
    ManageShortLinks -> W.tab (M.tab palette |> fullHeightTabs) {
      tabs = {
        selected = Just (clampToTabRange model.display.page),
        options = [ "Main", "Manage" ] |> List.map (\name -> {text = name, icon = always none}),
        onSelect = \s -> Just (DspSwitchPage (clampToTabRange s))},
      content = \s -> case s of
        Just 1 -> el [width fill, height fill, scrollbarY] (managePageView model palette)
        _ -> el [width fill, height fill, scrollbarY] (mainPageView model palette)
      }
    _ -> el [width fill, height fill, scrollbarY] (mainPageView model palette)
  ]

fullHeightTabs : W.TabStyle msg -> W.TabStyle msg
fullHeightTabs ({elementColumn, content} as style) = {style | elementColumn = height fill :: elementColumn, content = {content | content = height fill :: content.content}}

clampToTabRange : Int -> Int
clampToTabRange num = if num >= 0 && num < 2 then num else 0

topBarActions : ContentModel -> List (W.Button Msg)
topBarActions model = case model.auth of
  LoggedIn _ _ -> [{
    icon = always none,
    text = "Logout",
    onPress = Just MsgLogout}, darkModeButton model]
  Anonymous _ -> [{
    icon = always none,
    text = "Login",
    onPress = Just (DspLoginVisible True)}, darkModeButton model]

darkModeButton : ContentModel -> W.Button Msg
darkModeButton model = {
    icon = if model.display.darkMode then icon I.moon else icon I.sun,
    text = "",
    onPress = Just (DspDarkMode (not model.display.darkMode))}

mainPageView : ContentModel -> M.Palette -> Element Msg
mainPageView model palette = if model.role |> allows CreateAnonymousShortLinks then mainPageContent model palette else none

mainPageContent : ContentModel -> M.Palette -> Element Msg
mainPageContent model palette = column [centerX, centerY, spacing 5] [
  if model.role |> allows CreateNamedShortLinks
  then
    row [width fill, spacing 5] [
      el [ alignLeft ] (text "Link Id:"),
      el [ alignRight ] (W.textInput (M.textInput palette) {
        chips = [],
        text = model.shortLink.name,
        placeholder = Nothing,
        label = "Link Id",
        onChange = MsgChangeLinkIdInput})]
  else none,
  row [width fill, spacing 5] [
    el [ alignLeft ] (text "URL:"),
    el [ alignRight ] (W.textInput (M.textInput palette) {
      chips = [],
      text = model.shortLink.url,
      placeholder = Nothing,
      label = "URL",
      onChange = MsgChangeUrlInput})],
  row [centerX, spacing 5] [
    W.textButton (M.containedButton palette) {
      text = "Create",
      onPress = Just MsgCreate},
    if model.role |> allows ManageShortLinks
    then
      W.textButton (M.containedButton palette) {
        text = "Revise",
        onPress = Just MsgReplace}
    else none]]

managePageView : ContentModel -> M.Palette -> Element Msg
managePageView model palette = column [width fill, spacing 5, paddingXY 10 0] (Dict.toList model.linkMap |> List.map (managePageRow palette))

managePageRow : M.Palette -> (LinkId, URL) -> Element Msg
managePageRow palette (linkId, url) = row [width fill, spacing 5] [
  el [width (px 150), clipY] (text linkId), el [width fill, clipY, paddingXY 5 0] (text url),
  W.textButton (M.containedButton palette) {text = "Edit", onPress = Just (MsgRevise linkId)},
  W.textButton (M.containedButton palette) {text = "Delete", onPress = Just (MsgDelete linkId)}]

loginDialog : LoginForm -> M.Palette -> W.Modal Msg
loginDialog form palette = {
  onDismiss = Just (DspLoginVisible False),
  content = column [centerX, centerY, spacing 5, padding 15, Border.rounded 5, Background.color (color palette.background)] [
    row [width fill, spacing 5] [
      el [ alignLeft ] (text "Username:"),
      el [ alignRight ] (W.usernameInput (M.textInput palette) {
        chips = [],
        text = form.username,
        placeholder = Nothing,
        label = "Username",
        onChange = MsgChangeUsernameInput})],
    row [width fill, spacing 5] [
      el [ alignLeft ] (text "Password:"),
      el [ alignRight ] (W.currentPasswordInputV2 (M.passwordInput palette) {
        text = form.password,
        placeholder = Nothing,
        label = "Password",
        onChange = MsgChangePasswordInput,
        show = False})],
    row [centerX, spacing 5] [
      W.textButton (M.outlinedButton palette) {
        text = "Close",
        onPress = Just (DspLoginVisible False)},
      W.textButton (M.containedButton palette) {
        text = "Login",
        onPress = Just MsgLogin}
    ]
  ]}
