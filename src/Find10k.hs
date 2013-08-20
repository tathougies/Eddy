{-# LANGUAGE OverloadedStrings #-}
import System.Environment

import Control.Monad
import Control.Monad.Trans

import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Binary (sinkFile)
import Data.Conduit.Text (encode, utf8)
import Data.Maybe
import Data.Text
import Data.XML.Types

import Network.URI
import Network.HTTP.Conduit

import Text.Printf
import Text.XML
import Text.XML.Stream.Parse

feedUri :: Int -> Int -> String
feedUri month year = printf "http://www.sec.gov/Archives/edgar/monthly/xbrlrss-%04d-%02d.xml" year month

main :: IO ()
main = do
  [yearS, monthS] <- getArgs
  let year = read yearS :: Int
      month = read monthS :: Int
      uri = feedUri month year

      -- put :: Sink IO ()
      -- put = await >>= \x ->
      --       case x of
      --         Nothing -> return ()
      --         Just x -> do
      --                liftIO $ putStrLn x
      --                put

      -- doParse :: Conduit Event (ResourceT IO) Text
      -- doParse = do
      --    test <- force "rss required" parseRss
      --    case test of
      --      Just t -> forM_ t $ \t' ->
      --                case t' of
      --                  Just t' -> yield t'
      --                  Nothing -> return ()
      --      Nothing -> return ()
      --    return ()
      -- parseRss = tagName "rss" ignoreAttrs (const parseChannel)
      -- parseChannel = tagNoAttr "channel" $ many item
      -- item = tagNoAttr "item" title
      -- title = tagNoAttr "title" (content)

      itemLookup ["item", "channel", "rss"] = parseRss' [("item", [])] titleLookup >> return False
      itemLookup _ = return True

      titleLookup ("title":_) = content >>= yield >> return True
      titleLookup ("description":_) = content >>= yield >> return True
      titleLookup _ = return True

      parseRss = parseRss' [] itemLookup
      parseRss' stack fn = do
          x <- await
          case x of
            Nothing -> return ()
            Just (EventBeginElement name attrs) -> do
                let stack' = (name,attrs):stack
                push <- fn (Prelude.map (unpack . nameLocalName . fst) stack')
                if push
                  then parseRss' stack' fn
                  else parseRss' stack fn
            Just (EventEndElement name)
                | (not . Prelude.null $ stack) && name == (fst . Prelude.head $ stack) ->
                    case Prelude.tail stack of
                      [] -> return ()
                      stack' -> parseRss' stack' fn
                | otherwise -> fail ("Unclosed element " ++ show stack ++ " " ++ show name)
            Just _ -> parseRss' stack fn

  putStrLn uri
  request <- parseUrl uri
  (withManager :: (Manager -> ResourceT IO ()) -> IO ()) $ \manager -> do
      response <- http request manager
      _ <- responseBody response $$+- (parseBytes def =$ parseRss =$ CL.map (`append` (singleton '\n')) =$ encode utf8 =$ sinkFile "test.out")
      return ()
  return ()