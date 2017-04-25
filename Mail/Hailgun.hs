{-# LANGUAGE FlexibleContexts #-}
-- | Hailgun is a Haskell wrapper around the <http://documentation.mailgun.com/api_reference.html Mailgun api's> that use
-- type safety to ensure that you are sending a valid request to the Mailgun API's. Mailgun is a
-- service that lets you send emails. It also contains a number of other email handling API's that
-- will be implimented in the future.
module Mail.Hailgun
   ( sendEmail
   , hailgunMessage
   , hailgunMessageWReplyTo
   , addAttachment
   , HailgunContext(..)
   , HailgunMessage
   , MessageSubject
   , MessageContent(..)
   , MessageRecipients(..)
   , emptyMessageRecipients
   , UnverifiedEmailAddress
   , HailgunSendResponse(..)
   , HailgunErrorMessage
   , HailgunErrorResponse(..)
   , getDomains
   , Page(..)
   , HailgunDomain(..)
   , HailgunDomainResponse(..)
   , HailgunTime(..)
   , toProxy
   , Attachment(..)
   , AttachmentBody(..)
   ) where

import           Mail.Hailgun.Attachment
import           Mail.Hailgun.Domains
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import           Mail.Hailgun.Message
import           Mail.Hailgun.Pagination
import           Mail.Hailgun.SendEmail

import qualified Data.ByteString.Char8      as BC
import           Network.HTTP.Client        (Proxy (..))

-- | A convinience method to create a proxy for the HailgunContext.
toProxy
   :: String -- ^ The proxy host
   -> Int -- ^ The proxy port
   -> Proxy -- ^ The proxy configuration.
toProxy host = Proxy (BC.pack host)
