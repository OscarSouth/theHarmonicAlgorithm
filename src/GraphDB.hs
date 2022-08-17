{-# LANGUAGE OverloadedStrings #-}

module GraphDB where

import Data.Aeson
import Network.HTTP.Client

-- buildRequest :: String -> RequestBody -> IO Request
-- buildRequest url body = do
--   nakedRequest <- parseRequest url
--   return (nakedRequest { method = "POST", requestBody = body })

-- send :: RequestBody -> IO ()
-- send s = do
--   manager <- newManager defaultManagerSettings
--   request <- buildRequest "http://httpbin.org/post" s
--   response <- httpLbs request manager
--   let Just obj = decode (responseBody response)
--   print (obj :: Object)