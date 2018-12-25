{-# LANGUAGE FlexibleContexts #-}

module Sugina.AuthHardcoded (authHardcodedAlter) where

import Yesod
import Yesod.Auth
import Yesod.Auth.Hardcoded
import qualified Yesod.Auth.Message as Msg
import Yesod.Form (ireq, runInputPost, textField)

authHardcodedAlter :: YesodAuthHardcoded m => AuthPlugin m
authHardcodedAlter = AuthPlugin "hardcoded" dispatch loginWidget
 where
  dispatch "POST" ["login"] = postLogin'R >>= sendResponse
  dispatch _ _ = notFound
  loginWidget toMaster = do
    request <- getRequest
    [whamlet|$newline never
    <form method="post" action="@{toMaster loginR}">
      $maybe t <- reqToken request
        <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
      <p>
        <b>_{Msg.UserName}: 
        <input type="text" name="username" required>
      <p>
        <b>_{Msg.Password}: 
        <input type="password" name="password" required>
      <p>
        <button type="submit" .btn .btn-success>_{Msg.LoginTitle}
    |]
    toWidgetHead [hamlet|<meta name="viewport" content="width=device-width, initial-scale=1.0">|]
    addStylesheetRemote "/pubdyn/transform.css"

postLogin'R :: YesodAuthHardcoded site => AuthHandler site TypedContent
postLogin'R = do
  (username, password) <- runInputPost $ (,)
    <$> ireq textField "username"
    <*> ireq textField "password"
  isValid <- validatePassword username password
  if isValid
    then setCredsRedirect (Creds "hardcoded" username [])
    else do
      isExists <- doesUserNameExist username
      loginErrorMessageI LoginR
        (if isExists
          then Msg.InvalidUsernamePass
          else Msg.IdentifierNotFound username)
