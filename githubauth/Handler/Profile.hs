module Handler.Profile where

import Import
import GitHub.Endpoints.Users

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        Right usr <- liftIO $ userInfoFor "qizen"
        let name = userLogin usr
        setTitle . toHtml $ userIdent user <> "'s User Page"
        $(widgetFile "profile")
