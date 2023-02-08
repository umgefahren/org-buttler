{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Text (Text)
import Lib (loadAllOrgTasks)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

app :: Application
app req f = do
  let path = pathInfo req
  resp <- matchResponse path
  f resp

matchResponse :: [Text] -> IO Response
matchResponse ["api", "org", "tasks"] = orgTaskRoute
matchResponse _ = return anyRoute

anyRoute :: Response
anyRoute =
  responseLBS
    status404
    [(hContentType, "application/json")]
    "Not found"

orgTaskRoute :: IO Response
orgTaskRoute = do
  orgDir <- getEnv "ORGDIR"
  tasks <- loadAllOrgTasks orgDir
  let resp = responseLBS status200 [(hContentType, "application/json")] (encode tasks)
  return resp
