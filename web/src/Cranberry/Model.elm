module Cranberry.Model exposing (..)

import Dict exposing (Dict)
import Http

type Model = Undecided | Failed | Model ContentModel
type alias ContentModel = {
  notifications : List Notification,
  auth : Authentication,
  role : Role,
  shortLink : ShortLinkForm,
  linkMap : ShortLinkMap,
  display: DisplayPrefs}

type Notification = Info String | Failure String
type alias LoginForm = { username : String, password : String }
type alias ShortLinkForm = { name : String, url : String }
type Role = NoPermission | CreateAnonymousShortLinks | CreateNamedShortLinks | ManageShortLinks
type Authentication = Anonymous LoginForm | LoggedIn String Token

allows : Role -> Role -> Bool
allows min permission = case min of
  NoPermission -> True
  CreateAnonymousShortLinks -> List.member permission [ CreateAnonymousShortLinks, CreateNamedShortLinks, ManageShortLinks ]
  CreateNamedShortLinks -> List.member permission [ CreateNamedShortLinks, ManageShortLinks ]
  ManageShortLinks -> permission == ManageShortLinks

type alias DisplayPrefs = {
  darkMode : Bool,
  page : Int,
  showLoginDialog : Bool}

defaultDisplay : DisplayPrefs
defaultDisplay = {
  darkMode = False,
  page = 0,
  showLoginDialog = False}

type alias ShortLinkMap = Dict LinkId URL
type alias LinkId = String
type alias URL = String
type Token = Token String

type alias HttpMaybe a = Result Http.Error a

type Msg = MsgLogin | MsgLogout | MsgCreate | MsgReplace | MsgRevise LinkId | MsgDelete LinkId
  | MsgChangeUsernameInput String | MsgChangePasswordInput String | MsgChangeLinkIdInput LinkId 
  | MsgChangeUrlInput URL | MsgDiscardNotification Int | MsgRefreshLinkList | RspLogin (HttpMaybe PayloadLogin)
  | RspLogout (HttpMaybe ()) | RspMe (HttpMaybe PayloadMe) | RspListShortLinks (HttpMaybe ShortLinkMap)
  | RspCreate (HttpMaybe URL) | RspRevise (HttpMaybe URL) | RspDelete (HttpMaybe URL) | DspDarkMode Bool
  | DspSwitchPage Int | DspLoginVisible Bool

type alias PayloadLogin = { token : String, me : PayloadMe }
type alias PayloadMe = { user : Maybe String, role : Role }
