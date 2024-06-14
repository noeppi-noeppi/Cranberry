module Cranberry.Logic exposing (..)

import Cranberry.Model exposing (..)
import Cranberry.Request exposing (..)
import Dict
import Http

init : () -> (Model, Cmd Msg)
init _ = (Undecided, requestMeAnonymous)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model) of
  (_, Failed) -> (Failed, Cmd.none)
  (RspMe (Ok me), Undecided) -> case initContent me.role of
    (newContent, cmd) -> (Model newContent, cmd)
  (RspMe (Err _), Undecided) -> (Failed, Cmd.none)
  (_, Undecided) -> (Undecided, Cmd.none)
  (_, Model content) -> case updateContent msg content of
    (newContent, cmd) -> (Model newContent, cmd)

initContent : Role -> (ContentModel, Cmd Msg)
initContent role = let content = { notifications = [], auth = Anonymous (LoginForm "" ""), role = role, shortLink = ShortLinkForm "" "", linkMap = Dict.empty, display = defaultDisplay}
  in case role of
    ManageShortLinks -> (content, requestShortLinkMap content)
    _ -> (content, Cmd.none)

notify : ContentModel -> Notification -> ContentModel
notify model notification = {model | notifications = notification :: model.notifications}

maybeRefreshLinkMap : ContentModel -> Cmd Msg
maybeRefreshLinkMap model = case model.role of
  ManageShortLinks -> requestShortLinkMap model
  _ -> Cmd.none

reportError : ContentModel -> Http.Error -> (ContentModel, Cmd Msg)
reportError model err = case err of
  Http.BadUrl _ -> (Failure "Invalid request." |> notify model, Cmd.none)
  Http.Timeout -> (Failure "Network timeout." |> notify model, Cmd.none)
  Http.NetworkError -> (Failure "Network error." |> notify model, Cmd.none)
  Http.BadStatus 400 -> (Failure "Invalid request data." |> notify model, Cmd.none)
  Http.BadStatus 401 -> let newModel = {model | auth = Anonymous {username = "", password = ""}}
    in (Failure "You have been logged out." |> notify newModel, requestMe newModel)
  Http.BadStatus 403 -> (Failure "Forbidden." |> notify model, Cmd.none)
  Http.BadStatus 409 -> (Failure "Already exists." |> notify model, Cmd.none)
  Http.BadStatus code -> (Failure ("Request failure: " ++ String.fromInt code) |> notify model, Cmd.none)
  Http.BadBody errorMessage -> (Failure ("Unexpected response: " ++ errorMessage) |> notify model, Cmd.none)

updateContent : Msg -> ContentModel -> (ContentModel, Cmd Msg)
updateContent msg model = case (msg, model) of
  (MsgLogin, {auth, display}) -> case auth of
    Anonymous form -> ({model | auth = Anonymous {username = "", password = ""}, display = {display | showLoginDialog = False}}, requestLogin form.username form.password)
    LoggedIn _ _ -> (Failure "Already logged in." |> notify model, Cmd.none)
  (MsgLogout, {auth}) -> case auth of
    Anonymous _ -> (Failure "Not logged in." |> notify model, Cmd.none)
    LoggedIn _ _ -> (model, requestLogout model)
  (MsgCreate, {shortLink}) -> case (shortLink.name, shortLink.url) of
    (_, "") -> (Failure "Missing short link URL." |> notify model, Cmd.none)
    (name, url) -> ({model | shortLink = {name = "", url = ""}}, requestCreate model (if model.role |> allows CreateNamedShortLinks then name else "") url)
  (MsgReplace, {shortLink}) -> case (shortLink.name, shortLink.url) of
    ("", _) -> (Failure "Missing short link name." |> notify model, Cmd.none)
    (_, "") -> (Failure "Missing short link URL." |> notify model, Cmd.none)
    (name, url) -> ({model | shortLink = {name = "", url = ""}}, requestRevise model name url)
  (MsgRevise linkId, {linkMap, display}) -> case Dict.get linkId linkMap of
    Just url -> ({model | shortLink = {name = linkId, url = url}, display = {display | page = 0}}, Cmd.none)
    Nothing -> (Failure "Unknown short link to revise." |> notify model, Cmd.none)
  (MsgDelete linkId, _) -> (model, requestDelete model linkId)
  (MsgChangeUsernameInput input, {auth}) -> case auth of
    Anonymous editor -> ({model | auth = Anonymous {editor | username = input}}, Cmd.none)
    LoggedIn _ _ -> (model, Cmd.none)
  (MsgChangePasswordInput input, {auth}) -> case auth of
    Anonymous editor -> ({model | auth = Anonymous {editor | password = input}}, Cmd.none)
    LoggedIn _ _ -> (model, Cmd.none)
  (MsgChangeLinkIdInput input, {shortLink}) -> ({ model | shortLink = {shortLink | name = input} }, Cmd.none)
  (MsgChangeUrlInput input, {shortLink}) -> ({model | shortLink = {shortLink | url = input}}, Cmd.none)
  (MsgDiscardNotification idx, {notifications}) -> ({model | notifications = (List.take idx notifications) ++ (List.drop (idx + 1) notifications) }, Cmd.none)
  (MsgRefreshLinkList, _) -> (model, maybeRefreshLinkMap model)
  (RspMe (Ok me), _) -> ({model | role = me.role}, maybeRefreshLinkMap model)
  (RspLogin (Ok login), _) -> ({model | role = login.me.role, auth = LoggedIn (Maybe.withDefault "<unknown>" login.me.user) (Token login.token)}, maybeRefreshLinkMap model)
  (RspLogout (Ok ()), _) -> let newModel = {model | auth = Anonymous {username = "", password = ""}, role = NoPermission} in (newModel, requestMe newModel)
  (RspListShortLinks (Ok links), _) -> ({model | linkMap = links}, Cmd.none)
  (RspCreate (Ok url), _) -> (Info ("Link created: " ++ url) |> notify model, maybeRefreshLinkMap model)
  (RspRevise (Ok url), _) -> (Info ("Link revised: " ++ url) |> notify model, maybeRefreshLinkMap model)
  (RspDelete (Ok url), _) -> (Info ("Link deleted: " ++ url) |> notify model, maybeRefreshLinkMap model)
  (RspMe (Err _), _) -> (Failure "Could retrieve self" |> notify model, Cmd.none)
  (RspLogin (Err err), _) -> case err of
    Http.BadStatus 401 -> (Failure "Invalid credentials" |> notify model, Cmd.none)
    _ -> reportError model err
  (RspLogout (Err err), _) -> reportError model err
  (RspListShortLinks (Err err), _) -> reportError model err
  (RspCreate (Err err), _) -> reportError model err
  (RspRevise (Err err), _) -> reportError model err
  (RspDelete (Err err), _) -> reportError model err
  (DspDarkMode darkMode, {display}) -> ({model | display = {display | darkMode = darkMode}}, Cmd.none)
  (DspSwitchPage page, {display}) -> ({model | display = {display | page = page}}, maybeRefreshLinkMap model)
  (DspLoginVisible visible, {display}) -> ({model | display = {display | showLoginDialog = visible}}, Cmd.none)
