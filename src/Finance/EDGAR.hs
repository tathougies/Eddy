{-# LANGUAGE OverloadedStrings #-}
module Finance.EDGAR where

import Control.Applicative

import Data.Aeson
import Data.Text
import Data.Time

data Report = Report {
  rCompany :: Text,
  rType :: Text,
  rXBRLRef :: Text,
  rFilingTime :: UTCTime
  } deriving (Show)

instance ToJSON Report where
  toJSON (Report company typ xbrlRef filingTime) =
    object ["company" .= company,
            "reportType" .= typ,
            "xbrlRef" .= xbrlRef,
            "filingTime" .= filingTime]

instance FromJSON Report where
  parseJSON (Object v) = Report <$>
                         v .: "company" <*>
                         v .: "reportType" <*>
                         v .: "xbrlRef" <*>
                         v .: "filingTime"