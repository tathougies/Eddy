{-# LANGUAGE OverloadedStrings #-}
import Aws
import Aws.Core
import Aws.S3

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Binary as B (sourceHandle, lines)
import Data.Aeson
import Data.ByteString.Lazy
import Data.Text as T

import Finance.EDGAR

import Network.HTTP.Conduit

import System.Environment
import System.IO

main :: IO ()
main = do
  -- runResourceT $
  --   sourceHandle stdin $= B.lines $= CL.map (decode . fromChunks . (\x -> [x])) $= CL.catMaybes $= CL.mapM (liftIO . Prelude.putStrLn . Data.Text.unpack . rXBRLRef) $$ CL.sinkNull
  urls <- (runResourceT $
    sourceHandle stdin $= B.lines $= CL.map (decode . fromChunks . (\x -> [x])) $= CL.catMaybes $= CL.map rXBRLRef $$ CL.consume :: IO [Text])

  let serviceConf = s3 HTTPS s3EndpointUsClassic True
  awsConf <- baseConfiguration

  withManager $ \manager -> do
    forM_ (Prelude.zip [1..] urls) $ \(i,url) -> do
      request <- parseUrl (T.unpack url)
      let objectName = Prelude.last (T.split (=='/') url)
      liftIO (Prelude.putStrLn (Prelude.concat ["Downloading ", T.unpack objectName, "(",show i," of ",show (Prelude.length urls), ")"]))
      response <- http request manager
      dat <- fromChunks <$> (responseBody response $$+- CL.consume)
      let s3ObjectBody = RequestBodyLBS dat
          s3PutCmd = putObject "eddy-sec-edgar" (T.concat ["reports/", objectName]) s3ObjectBody
      liftIO (Prelude.putStrLn (T.unpack . T.concat $ ["Uploading to S3(", objectName,")..."]))
      pureAws awsConf serviceConf manager s3PutCmd