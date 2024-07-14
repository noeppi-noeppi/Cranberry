module Cranberry.View exposing (..)

import Cranberry.Model exposing (..)
import Color
import Html
import Html.Attributes
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

type alias Dimension = { width: Int, height: Int }
type alias LayoutArgs = { viewport : Dimension, deviceClass : DeviceClass, palette: M.Palette }

color : Color.Color -> Element.Color
color c = case Color.toRgba c of
  {red, green, blue, alpha} -> rgba red green blue alpha

colorWithAlpha : Color.Color -> Float -> Element.Color
colorWithAlpha c alpha = case Color.toRgba c of
  {red, green, blue} -> rgba red green blue alpha

icon : I.Icon -> Widget.Icon.Icon msg
icon ic = Widget.Icon.elmFeather I.toHtml ic

fadeOutMask : List (Element.Attribute msg)
fadeOutMask = List.map htmlAttribute [
  Html.Attributes.style "mask-mode" "alpha",
  Html.Attributes.style "mask-image" "linear-gradient(270deg,transparent,white 2em)"]

doLayout : LayoutArgs -> Element msg -> Element msg -> Maybe (W.Modal msg) -> Html.Html msg
doLayout args root notifs modal =
  let viewportAttrs = [width fill, height fill] in
  let paletteAttrs = [Background.color (color args.palette.background), Font.color (color args.palette.on.background)] in
  let modalAttrs = (modal |> Maybe.map (\m -> W.singleModal [m]) |> Maybe.withDefault []) in
    root |> layout (viewportAttrs ++ paletteAttrs ++ modalAttrs ++ [inFront notifs])

view : Model -> Html.Html Msg
view model = case model of
  Undecided _ -> el [centerX, centerY] (W.circularProgressIndicator (M.progressIndicator M.defaultPalette) Nothing) |> layout []
  Failed -> el [centerX, centerY] (text "Something went wrong.") |> layout []
  Model flags content -> let viewport = { width = flags.viewportWidth, height = flags.viewportHeight } in modalView content {
    viewport = viewport,
    deviceClass = (classifyDevice viewport).class,
    palette = if content.display.darkMode then M.darkPalette else M.defaultPalette}

modalView : ContentModel -> LayoutArgs -> Html.Html Msg
modalView model args = case model.auth of
  Anonymous form -> if model.display.showLoginDialog
    then doLayout args (pageView model args) (notifications model args) (Just (loginDialog form args))
    else doLayout args (pageView model args) (notifications model args) Nothing
  _ -> doLayout args (pageView model args) (notifications model args) Nothing

notifications : ContentModel -> LayoutArgs -> Element Msg
notifications model args = model.notifications |> notifList 0 args |> column [padding 3, spacing 3, width (ifMobile args fill shrink), alignRight, alignBottom] 

notifList : Int -> LayoutArgs -> List Notification -> List (Element Msg)
notifList idx args list = case list of
  (Info msg) :: xs -> notifListItem msg idx args args.palette.secondary args.palette.on.secondary :: notifList (idx + 1) args xs
  (Failure msg) :: xs -> notifListItem msg idx args args.palette.error args.palette.on.error :: notifList (idx + 1) args xs
  [] -> []

notifListItem : String -> Int -> LayoutArgs -> Color.Color -> Color.Color -> Element Msg
notifListItem msg idx args bg fg = row [
  width (ifMobile args (fill) (minimum 300 shrink)), alignRight, padding 10, spacing 3, Border.rounded 5,
  Background.color (color bg), Font.color (color fg)] [
     (case args.deviceClass of
       Phone -> el [alignLeft, padding 5, width fill, Font.size 16, scrollbarX, htmlAttribute (Html.Attributes.style "margin" "-5px")] (text msg)
       _ -> el [alignLeft, padding 0] (text msg)
     ),
     Input.button [alignRight, padding 0] {onPress = Just (MsgDiscardNotification idx), label = I.toHtml [] I.x |> html}]

pageView : ContentModel -> LayoutArgs -> Element Msg
pageView model args = column [ width fill, height fill ] [
  el [width fill] (W.menuBar (M.menuBar args.palette) {
    title = (case model.auth of
      LoggedIn name _ -> text ("Cranberry - " ++ name)
      Anonymous _ -> text "Cranberry"),
    deviceClass = args.deviceClass,
    openLeftSheet = Nothing,
    openRightSheet = Nothing,
    openTopSheet = Nothing,
    search = Nothing,
    primaryActions = topBarActions model}),
  case model.role of
    ManageShortLinks -> W.tab (M.tab args.palette |> scrollingTabs) {
      tabs = {
        selected = Just (clampToTabRange model.display.page),
        options = [ "Main", "Manage" ] |> List.map (\name -> {text = name, icon = always none}),
        onSelect = \s -> Just (DspSwitchPage (clampToTabRange s))},
      content = \s -> case s of
        Just 1 -> managePageView model args
        _ -> mainPageView model args
      }
    _ -> mainPageView model args
  ]

scrollingTabs : W.TabStyle msg -> W.TabStyle msg
scrollingTabs ({elementColumn, content} as style) = {style | elementColumn = height fill :: elementColumn, content = {content | content = height fill :: scrollbarY :: content.content}}

clampToTabRange : Int -> Int
clampToTabRange num = if num >= 0 && num < 2 then num else 0

topBarActions : ContentModel -> List (W.Button Msg)
topBarActions model = case model.auth of
  LoggedIn _ _ -> [{
    icon = icon I.user,
    text = "Logout",
    onPress = Just MsgLogout}, darkModeButton model]
  Anonymous _ -> [{
    icon = icon I.user,
    text = "Login",
    onPress = Just (DspLoginVisible True)}, darkModeButton model]

darkModeButton : ContentModel -> W.Button Msg
darkModeButton model = {
    icon = if model.display.darkMode then icon I.moon else icon I.sun,
    text = if model.display.darkMode then "Dark" else "Light",
    onPress = Just (DspDarkMode (not model.display.darkMode))}

mainPageView : ContentModel -> LayoutArgs -> Element Msg
mainPageView model args = if model.role |> allows CreateAnonymousShortLinks then mainPageContent model args else none

mainPageContent : ContentModel -> LayoutArgs -> Element Msg
mainPageContent model args = column [centerX, centerY, spacing 5] [
  if model.role |> allows CreateNamedShortLinks
  then
    W.textInput (M.textInput args.palette) {
      chips = [],
      text = model.shortLink.name,
      placeholder = Nothing,
      label = "Link Id",
      onChange = MsgChangeLinkIdInput} |> labeledInput args "Link Id"
  else none,
  W.textInput (M.textInput args.palette) {
    chips = [],
    text = model.shortLink.url,
    placeholder = Nothing,
    label = "URL",
    onChange = MsgChangeUrlInput} |> labeledInput args "URL",
  row [centerX, spacing 5] [
    W.textButton (M.containedButton args.palette) {
      text = "Create",
      onPress = Just MsgCreate},
    if model.role |> allows ManageShortLinks
    then
      W.textButton (M.containedButton args.palette) {
        text = "Revise",
        onPress = Just MsgReplace}
    else none]]

managePageView : ContentModel -> LayoutArgs -> Element Msg
managePageView model args = let separator = el [width fill, paddingXY 0 1, Background.color (colorWithAlpha args.palette.on.background 0.05)] none
  in column [width fill, spacing 5, paddingXY 10 0] (Dict.toList model.linkMap |> List.map (managePageRow args) |> interleave separator |> \l -> l ++ [el [paddingXY 5 0] none])

managePageRow : LayoutArgs -> (LinkId, URL) -> Element Msg
managePageRow args (linkId, url) = case args.deviceClass of
  Phone -> row [width fill, spacing 5] [
    column ([width fill, clipX, padding 5, spacing 4, centerY] ++ fadeOutMask) [
      el [Font.size 18] (text linkId),
      el [Font.size 18] (text url)],
    managePageRowButton args "Edit" (icon I.edit) (MsgRevise linkId),
    managePageRowButton args "Delete" (icon I.trash2) (MsgDelete linkId)]
  _ -> row [width fill, spacing 5] [
    el ([width (fillPortion 1), clipX, padding 5] ++ fadeOutMask) (text linkId),
    el ([width (fillPortion 5), clipX, padding 5] ++ fadeOutMask) (text url),
    managePageRowButton args "Edit" (icon I.edit) (MsgRevise linkId),
    managePageRowButton args "Delete" (icon I.trash2) (MsgDelete linkId)]

managePageRowButton : LayoutArgs -> String -> Widget.Icon.Icon Msg -> Msg -> Element Msg
managePageRowButton args title btnIcon message = case args.deviceClass of
  Phone -> W.iconButton (M.containedButton args.palette) {text = title, icon = btnIcon, onPress = Just message}
  _ -> W.textButton (M.containedButton args.palette) {text = title, onPress = Just message}

loginDialog : LoginForm -> LayoutArgs -> W.Modal Msg
loginDialog form args = {
  onDismiss = Just (DspLoginVisible False),
  content = column [centerX, centerY, spacing 5, padding 15, Border.rounded 5, Background.color (color args.palette.background)] [
    W.usernameInput (M.textInput args.palette) {
      chips = [],
      text = form.username,
      placeholder = Nothing,
      label = "Username",
      onChange = MsgChangeUsernameInput} |> labeledInput args "Username",
    W.currentPasswordInputV2 (M.passwordInput args.palette) {
      text = form.password,
      placeholder = Nothing,
      label = "Password",
      onChange = MsgChangePasswordInput,
      show = False} |> labeledInput args "Password",
    row [centerX, spacing 5] [
      W.textButton (M.outlinedButton args.palette) {
        text = "Close",
        onPress = Just (DspLoginVisible False)},
      W.textButton (M.containedButton args.palette) {
        text = "Login",
        onPress = Just MsgLogin}
    ]
  ]}

labeledInput : LayoutArgs -> String -> Element Msg -> Element Msg
labeledInput args title input = case args.deviceClass of
  Phone -> column [width fill, spacing 5, paddingXY 5 0] [
    el [alignLeft, Font.size 15] (title ++ ":" |> text),
    el [ alignLeft, width fill ] input,
    el [paddingXY 0 4] none]
  _ -> row [width fill, spacing 5] [
    el [ alignLeft ] (title ++ ":" |> text),
    el [ alignRight ] input]

ifMobile : LayoutArgs -> a -> a -> a
ifMobile args mobile desktop = case args.deviceClass of
  Phone -> mobile
  _ -> desktop

interleave : a -> List a -> List a
interleave e list = case list of
  [] -> []
  x :: [] -> x :: []
  x :: xs -> x :: e :: interleave e xs
