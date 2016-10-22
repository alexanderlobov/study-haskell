{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)

app :: Application
app request respond = respond $ case rawPathInfo request of
    "/" -> index
    "/raw/" -> plainIndex
    _ -> notFound

pathToIndex = "/home/alex/src/git/study-haskell/wai-try/index.html"

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    pathToIndex
    Nothing

plainIndex :: Response
plainIndex = responseFile
    status200
    [("Content-Type", "text/plain")]
    pathToIndex
    Nothing

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 (logStdout app)
