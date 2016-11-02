module Handler.Blog
    ( getBlogR
    , postBlogR
    )
where

import Import
import Yesod.Form.Nic (YesodNic, nicHtmlField)

instance YesodNic App

entryForm :: Form Article
entryForm = renderDivs $ Article
    <$> areq textField "Title" Nothing
    <*> areq nicHtmlField "Content" Nothing

getBlogR :: Handler Html
getBlogR = do
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "articles")

postBlogR :: Handler Html
postBlogR = error "Not yet implemented: postBlogR"
