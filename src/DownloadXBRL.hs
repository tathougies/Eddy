{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Aws
import Aws.Core
import Aws.S3

import qualified Control.Exception.Lifted as EX
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Zlib as Z
import Data.Conduit.Binary as B (sourceHandle, lines)
import Data.Aeson
import Data.ByteString.Lazy
import Data.Text as T

import Finance.EDGAR

import Network.HTTP.Conduit

import System.Environment
import System.IO

attempting :: Int -> ResourceT IO a -> ResourceT IO a
attempting 1 action = action `EX.catch` (\(e::EX.SomeException) -> fail ("Attempts failed; quitting on " ++ show e))
attempting n action = action `EX.catch` (\(e::EX.SomeException) -> do
                                            liftIO (Prelude.putStrLn ("Retrying... (Error was: " ++ show e ++ ")"))
                                            attempting (n - 1) action)

main :: IO ()
main = do
  -- runResourceT $
  --   sourceHandle stdin $= B.lines $= CL.map (decode . fromChunks . (\x -> [x])) $= CL.catMaybes $= CL.mapM (liftIO . Prelude.putStrLn . Data.Text.unpack . rXBRLRef) $$ CL.sinkNull
  urlsAndCiks <- (runResourceT $
    sourceHandle stdin $= B.lines $= CL.map (decode . fromChunks . (\x -> [x])) $= CL.catMaybes $= CL.map (\x -> (rXBRLRef x, rCIKNumber x)) $$ CL.consume :: IO [(Text, Text)])

  let serviceConf = s3 HTTPS s3EndpointUsClassic True
  awsConf <- baseConfiguration

  withManager $ \manager -> do
    forM_ (Prelude.zip [1..] urlsAndCiks) $ \(i,(url, cik)) -> do
      request <- parseUrl (T.unpack url)
      let objectName = Prelude.last (T.split (=='/') url)
      liftIO (Prelude.putStrLn (Prelude.concat ["Downloading ", T.unpack objectName, "(",show i," of ",show (Prelude.length urlsAndCiks), ")"]))
      dat <- attempting 3 $ do
        response <- http request manager
        fromChunks <$> (responseBody response $$+- (Z.compress 9 (WindowBits 31) =$ CL.consume))
      let s3ObjectBody = RequestBodyLBS dat
          s3Loc = T.concat ["reports/", cik ,"/", objectName, ".gz"]
          s3PutCmd' = putObject "eddy-sec-edgar" s3Loc s3ObjectBody
          s3PutCmd = s3PutCmd' { poAcl = Just AclPublicRead }
      liftIO (Prelude.putStrLn (T.unpack . T.concat $ ["Uploading to S3(", s3Loc,")..."]))
      attempting 3 (pureAws awsConf serviceConf manager s3PutCmd)