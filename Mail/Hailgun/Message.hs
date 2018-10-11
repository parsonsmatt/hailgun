module Mail.Hailgun.Message
    ( hailgunMessage
    ) where

import qualified Data.ByteString.Char8            as BC
import           Data.List                        (find)
import           Mail.Hailgun.Attachment.Internal
import           Mail.Hailgun.AttachmentsSearch
import           Mail.Hailgun.Internal.Data
import           Text.Email.Validate

-- | A method to construct a HailgunMessage. You require a subject, content, From address and people
-- to send the email to and it will give you back a valid Hailgun email message. Or it will error
-- out while trying.
hailgunMessage
   :: MessageSubject -- ^ The purpose of the email surmised.
   -> MessageContent -- ^ The full body of the email.
   -> UnverifiedEmailAddress -- ^ The email account that the recipients should respond to in order to get back to us.
   -> MessageRecipients -- ^ The people that should recieve this email.
   -> [Attachment] -- ^ The attachments that you want to attach to the email; standard or inline.
   -> Either HailgunErrorMessage HailgunMessage -- ^ Either an error while trying to create a valid message or a valid message.
hailgunMessage subject content sender recipients simpleAttachments = do
   from  <- validate sender
   to    <- mapM validate (recipientsTo recipients)
   cc    <- mapM validate (recipientsCC recipients)
   bcc   <- mapM validate (recipientsBCC recipients)
   attachments <- attachmentsInferredFromMessage content cleanAttachments
   return HailgunMessage
      { messageSubject = subject
      , messageContent = content
      , messageFrom = from
      , messageTo = to
      , messageCC = cc
      , messageBCC = bcc
      , messageAttachments = attachments
      }
   where
      cleanAttachments = fmap cleanAttachmentFilePath simpleAttachments

attachmentsInferredFromMessage :: MessageContent -> [Attachment] -> Either String [SpecificAttachment]
attachmentsInferredFromMessage mContent simpleAttachments =
   case mContent of
      (TextOnly _) -> return . fmap toStandardAttachment $ simpleAttachments
      th@(TextAndHTML {}) -> convertAttachments simpleAttachments (findInlineImagesInHtmlEmail . htmlContent $ th)

convertAttachments :: [Attachment] -> [InlineImage] -> Either String [SpecificAttachment]
convertAttachments attachments images = do
   inlineAttachments <- sequence (fmap (findAttachmentForImage attachments) images)
   let standardAttachments = toStandardAttachment <$> attachments `notInSpecific` inlineAttachments
   return $ inlineAttachments ++ standardAttachments

notInSpecific :: [Attachment] -> [SpecificAttachment] -> [Attachment]
notInSpecific simpleAttachments specificAttachments =
   filter (\sa -> attachmentFilePath sa `notElem` specificFilePaths) simpleAttachments
   where
      specificFilePaths = fmap saFilePath specificAttachments

findAttachmentForImage :: [Attachment] -> InlineImage -> Either String SpecificAttachment
findAttachmentForImage attachments image =
   case find (`attachmentForInlineImage` image) attachments of
      Nothing         -> Left . missingInlineImageErrorMessage $ image
      Just attachment -> Right . toInlineAttachment $ attachment

missingInlineImageErrorMessage :: InlineImage -> String
missingInlineImageErrorMessage image =
   "Could not find an attachment for the inline image: "
   ++ (show . imageSrc $ image)
   ++ ". Either provide the attachment or remove the inline image from the HTML email."

attachmentForInlineImage :: Attachment -> InlineImage -> Bool
attachmentForInlineImage attachment image = (BC.pack . attachmentFilePath $ attachment) == imageSrc image
