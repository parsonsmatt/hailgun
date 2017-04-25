module Mail.Hailgun.MailgunApi
    ( mailgunApiPrefix
    , mailgunApiPrefixContext
    ) where

import           Mail.Hailgun.Internal.Data

mailgunApiPrefix :: String
mailgunApiPrefix = "https://api.mailgun.net/v3"

mailgunApiPrefixContext :: HailgunContext -> String
mailgunApiPrefixContext context = mailgunApiPrefix ++ "/" ++ hailgunDomain context
