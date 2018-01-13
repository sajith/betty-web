module Betty.Signup
       (
         sendVerificationEmail
       ) where

#if USE_AWS_SES
import Betty.Signup.SES      (sendVerificationEmail)
#else
import Betty.Signup.Sendmail (sendVerificationEmail)
#endif
