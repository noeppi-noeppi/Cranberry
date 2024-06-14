module Cranberry.Request exposing (..)

import Cranberry.Model exposing (..)
import Http
import Url
import Base64
import Json.Decode as J

mimeUriList : String
mimeUriList = "text/uri-list;charset=UTF-8"

accept : Http.Header
accept = Http.header "Accept" "text/plain;charset=UTF-8, application/json, text/uri-list;charset=UTF-8"

decodeMe : J.Decoder PayloadMe
decodeMe = J.map2 PayloadMe
  (J.field "user" (J.nullable J.string))
  (J.field "role" J.string |> J.andThen (\str -> case str of
    "none" -> J.succeed NoPermission
    "create_anonymous" -> J.succeed CreateAnonymousShortLinks
    "create_named" -> J.succeed CreateNamedShortLinks
    "manage" -> J.succeed ManageShortLinks
    _ -> J.fail "Invalid role."
  ))

decodeLogin : J.Decoder PayloadLogin
decodeLogin = J.map2 PayloadLogin
  (J.field "token" J.string)
  (J.field "me" decodeMe)

decodeShortLinkMap : J.Decoder ShortLinkMap
decodeShortLinkMap = J.dict J.string

authenticateBasic : String -> String -> Http.Header
authenticateBasic username password = Http.header "Authorization" ("Basic " ++ Base64.encode (username ++ ":" ++ password))

authenticateToken : Token -> Http.Header
authenticateToken (Token token) = Http.header "Authorization" ("Bearer " ++ token)

authenticate : ContentModel -> List Http.Header
authenticate model = case model.auth of
  LoggedIn _ token -> [ authenticateToken token, Http.header "X-No-Authenticate" "1" ]
  _ -> [ Http.header "X-No-WWW-Authenticate" "1" ]

authenticatedGet : ContentModel -> {url : URL, expect : Http.Expect msg} -> Cmd msg
authenticatedGet model obj = Http.request {
  method = "GET",
  url = obj.url,
  headers = accept :: authenticate model,
  body = Http.emptyBody,
  expect = obj.expect,
  timeout = Nothing,
  tracker = Nothing}

authenticatedPost : ContentModel -> {url : URL, body : Http.Body, expect : Http.Expect msg} -> Cmd msg
authenticatedPost model obj = Http.request {
  method = "POST",
  url = obj.url,
  headers = accept :: authenticate model,
  body = obj.body,
  expect = obj.expect,
  timeout = Nothing,
  tracker = Nothing}

requestMeAnonymous : Cmd Msg
requestMeAnonymous = Http.request {
  method = "GET",
  url = "/_/api/me",
  headers = [ accept ],
  body = Http.emptyBody,
  expect = Http.expectJson RspMe decodeMe,
  timeout = Nothing,
  tracker = Nothing}

requestMe : ContentModel -> Cmd Msg
requestMe model = authenticatedGet model {
  url = "/_/api/me",
  expect = Http.expectJson RspMe decodeMe}

requestLogin : String -> String -> Cmd Msg
requestLogin username password = Http.request {
  method = "POST",
  url = "/_/api/grant",
  headers = [ accept, authenticateBasic username password ],
  body = Http.emptyBody,
  expect = Http.expectJson RspLogin decodeLogin,
  timeout = Nothing,
  tracker = Nothing}

requestLogout : ContentModel -> Cmd Msg
requestLogout model = authenticatedPost model {
  url = "/_/api/revoke",
  body = Http.emptyBody,
  expect = Http.expectWhatever RspLogout}

requestShortLinkMap : ContentModel -> Cmd Msg
requestShortLinkMap model = authenticatedGet model {
  url = "/_/api/list",
  expect = Http.expectJson RspListShortLinks decodeShortLinkMap}

requestCreate : ContentModel -> LinkId -> URL -> Cmd Msg
requestCreate model name url = authenticatedPost model {
  url = "/_/api/create" ++ (if name == "" then "" else "/" ++ Url.percentEncode name),
  body = Http.stringBody mimeUriList url,
  expect = Http.expectString RspCreate}

requestRevise : ContentModel -> LinkId -> URL -> Cmd Msg
requestRevise model name url = authenticatedPost model {
  url = "/_/api/revise/" ++ Url.percentEncode name,
  body = Http.stringBody mimeUriList url,
  expect = Http.expectString RspRevise}

requestDelete : ContentModel -> LinkId -> Cmd Msg
requestDelete model name = authenticatedPost model {
  url = "/_/api/delete/" ++ Url.percentEncode name,
  body = Http.emptyBody,
  expect = Http.expectString RspDelete}
