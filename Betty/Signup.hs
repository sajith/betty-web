module Betty.Signup
       (
         sendVerificationEmail
       ) where

#if (!USE_AWS_SES)
import Betty.Signup.Sendmail (sendVerificationEmail)
#else
import Betty.Signup.SES      (sendVerificationEmail)
#endif