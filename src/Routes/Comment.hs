{-# LANGUAGE OverloadedStrings #-}

module Routes.Comment where
import Data.Proxy (Proxy)
import           Server.Routes                  ( Route(..) )
import           Server.Handlers                 ( Handler )
import Models.Comment (Comment)

-- commentsRoutes :: [(Route, Handler)]
-- commentsRoutes = [
--     (getCommentsListRoute, list )
--     , (deleteCommentRoute remove (Proxy :: Proxy Comment))
-- ]

createCommentRoute :: Route
createCommentRoute =
    PathRoute "api" $ PathRoute "comments" $ MethodRoute "POST"

getCommentsListRoute :: Route
getCommentsListRoute =
    PathRoute "api" $ PathRoute "comments" $ MethodRoute "GET"

updateCommentRoute :: Route
updateCommentRoute =
    PathRoute "api" $ PathRoute "comments" $ DynamicRoute "pk" $ MethodRoute
        "PATCH"

deleteCommentRoute :: Route
deleteCommentRoute =
    PathRoute "api" $ PathRoute "comments" $ DynamicRoute "pk" $ MethodRoute
        "DELETE"
