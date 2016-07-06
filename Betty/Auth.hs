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

module Betty.Auth where

import ClassyPrelude.Yesod

import Yesod.Auth
import Yesod.Auth.Email

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
