------------------------------------------------------------------------
--
-- Some customizations to Yesod.Auth.Email.
--
-- Yesod.Auth.Email is good enough out of the box, although some
-- things could use some work: (1) the login form (it would be nice to
-- have hints about password retrieval and new accounts in the login
-- form, and the default style isn't that nice); (2) password reset
-- email (uses the same text as signup email?), etc.
--
------------------------------------------------------------------------

module Betty.Auth ( authEmailBetty
                  , localRegisterHandler
                  , localForgotPasswordHandler
                  ) where

import           ClassyPrelude.Yesod

import           Yesod.Auth
import           Yesod.Auth.Email

import qualified Yesod.Auth.Message  as Msg

------------------------------------------------------------------------

-- Customizations to the login form.
authEmailBetty :: YesodAuthEmail m => AuthPlugin m
authEmailBetty = do
    AuthPlugin "email" (apDispatch authEmail) $ \tm -> do
        request <- getRequest
        [whamlet|
<div #emailLoginFormOuter>
  <div #emailLoginForm>
    <form method=post action=@{tm loginR}>
      $maybe token <- reqToken request
        <input type=hidden name=#{defaultCsrfParamName} value=#{token}>
      <div>
        <input #email name=email required="" value="" autofocus="" placeholder=Email type=email>
      <div>
        <input #password name=password required="" value="" placeholder=Password type=password>
      <div>
        <button #submitButton .btn .btn-success type=submit>
          Login with email
      <div>
        <a #forgotPassword href=@{tm forgotPasswordR}>
          Forgot password?
        <br />
        <a #newAccount href=@{tm registerR}>
          Don't have an account?
|]

------------------------------------------------------------------------

localRegisterHandler :: YesodAuthEmail master => AuthHandler master Html
localRegisterHandler = do
    tp <- getRouteToParent
    request <- getRequest
    lift $ authLayout $ do
        setTitleI Msg.RegisterLong
        [whamlet|
            <p>#{registerMessage}
            <form method="post" action="@{tp registerR}">
              $maybe token <- reqToken request
                <input type=hidden name=#{defaultCsrfParamName} value=#{token}>
              <div id="registerForm">
                <div>
                  <input #email name=email required="" value="" autofocus="" placeholder=Email type=email>
              <button #submitButton .btn .btn-success>_{Msg.Register}
              <div>
                <a #accountExists href=@{tp loginR}>
                  Have an account?
                <br />
                <a #forgotPassword href=@{tp forgotPasswordR}>
                  Forgot password?
        |]

------------------------------------------------------------------------

registerMessage :: Text
registerMessage = unlines
    [ "To create a new account, enter your e-mail address below, and "
    , "you will receive an e-mail with a confirmation link. "
    , "Be sure to visit the link to complete registration!"
    ]

------------------------------------------------------------------------

localForgotPasswordHandler :: YesodAuthEmail master => AuthHandler master Html
localForgotPasswordHandler = do
    tp <- getRouteToParent
    -- email <- newIdent
    lift $ authLayout $ do
        request <- getRequest
        setTitleI Msg.PasswordResetTitle
        [whamlet|
            <p>#{forgotMessage}
            <form method="post" action="@{tp forgotPasswordR}">
              $maybe token <- reqToken request
                <input type=hidden name=#{defaultCsrfParamName} value=#{token}>
              <div id="registerForm">
                <div>
                  <input #email name=email required="" value="" autofocus="" placeholder=Email type=email>
              <button .btn .btn-success>_{Msg.SendPasswordResetEmail}
        |]

------------------------------------------------------------------------

forgotMessage :: Text
forgotMessage = unlines
    [ "Enter your e-mail address below, and you will receive "
    , "an e-mail with a password reset link. "
    , "Visit the link to reset your password."
    ]

------------------------------------------------------------------------
