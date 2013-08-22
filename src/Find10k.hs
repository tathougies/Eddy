{-# LANGUAGE OverloadedStrings #-}
import System.Environment

import Control.Monad
import Control.Monad.Trans

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson
import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Binary (sinkHandle)
import Data.Maybe
import Data.Text as T
import Data.XML.Types
import Data.Time

import Finance.EDGAR

import Network.URI
import Network.HTTP.Conduit

import System.IO
import System.Locale

import Text.Printf
import Text.XML
import Text.XML.Stream.Parse

feedUri :: Int -> Int -> String
feedUri month year = printf "http://www.sec.gov/Archives/edgar/monthly/xbrlrss-%04d-%02d.xml" year month

data FeedPart = CompanyStart |
                Company Text |
                ReportType Text |
                CIKNumber Text |
                XBRLRef Text |
                FilingTime UTCTime
 deriving Show

contentText :: Content -> [Text]
contentText (ContentText t) = [t]
contentText (ContentEntity entity) = [T.pack "&", entity, T.pack ";"]

contentFlat :: [Content] -> Text
contentFlat cs = T.concat (cs >>= contentText)

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

      itemLookup [("item", _), ("channel",_), ("rss",_)] = yield CompanyStart >> parseRss' [("item", [])] titleLookup >> return False
      itemLookup _ = return True

      descriptionName = "{http://www.sec.gov/Archives/edgar}description"
      typeName = "{http://www.sec.gov/Archives/edgar}type"
      urlName = "{http://www.sec.gov/Archives/edgar}url"

      titleLookup (("companyName",_):("xbrlFiling",_):_) = content >>= (yield . Company) >> return True
      titleLookup (("description",_):_) = content >>= (yield . ReportType) >> return True
      titleLookup (("cikNumber",_):_) = content >>= (yield . CIKNumber) >> return True
      titleLookup (("acceptanceDatetime", _):("xbrlFiling", _):_) = do
        x <- content
        let filingTime = parseTime defaultTimeLocale "%Y%m%d%k%M%S" (unpack x)
        case filingTime of
          Nothing -> return True
          Just time -> do
            yield (FilingTime time)
            return True
      titleLookup (("xbrlFile", attrs):("xbrlFiles",_):_) =
        case lookup typeName attrs of
          Just x
            | -- x == [ContentText "XBRL INSTANCE DOCUMENT"] ||
              -- x == [ContentText "EX-101 INSTANCE DOCUMENT"] ||
              -- x == [ContentText "XBRL INSTANCE FILE"] ||
              -- x == [ContentText "EXHIBIT 101.INS"] ||
              -- x == [ContentText "XBRL INSTANCE"]
              x == [ContentText "EX-101.INS"] ||
              x == [ContentText "EX-100.INS"] -> do
              case lookup urlName attrs of
                Nothing -> return ()
                Just url -> yield (XBRLRef (contentFlat url))
              return True
          _ -> return True
      titleLookup _ = return True

      collectCompanyInfo = collectCompanyInfo' Nothing
      collectCompanyInfo' r = do
        x <- await
        case x of
          Nothing -> return ()
          Just CompanyStart -> do
            maybe (return ()) yield r
            collectCompanyInfo' (Just (Report "" "" "" "" undefined))
          Just (Company t) -> do
            collectCompanyInfo' (Just ((fromJust r) {rCompany = t}))
          Just (ReportType t) -> collectCompanyInfo' (Just ((fromJust r) { rType = t }))
          Just (XBRLRef t) ->
            let Just r' = r
            in if rXBRLRef r' == ""
               then collectCompanyInfo' (Just (r' { rXBRLRef = t }))
               else collectCompanyInfo' r
          Just (FilingTime t) -> collectCompanyInfo' (Just ((fromJust r) { rFilingTime = t }))
          Just (CIKNumber n) -> collectCompanyInfo' (Just ((fromJust r) { rCIKNumber = n }))

      parseRss = parseRss' [] itemLookup
      parseRss' stack fn = do
          x <- await
          case x of
            Nothing -> return ()
            Just (EventBeginElement name attrs) -> do
                let stack' = (name,attrs):stack
                push <- fn (Prelude.map (\(name, attrs) -> (unpack . nameLocalName $ name, attrs)) stack')
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
  request <- parseUrl uri
  (withManager :: (Manager -> ResourceT IO ()) -> IO ()) $ \manager -> do
      response <- http request manager
      _ <- responseBody response $$+- (parseBytes def =$ parseRss =$ collectCompanyInfo =$ CL.map (BS.concat . LBS.toChunks . (`LBS.append` (LBS.singleton '\n') ) . encode) =$ sinkHandle stdout)
      return ()
  return ()