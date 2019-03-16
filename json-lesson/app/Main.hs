module Main where

import Control.Monad
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults  = results <$> noaaResponse
  printResults noaaResults

data Book = Book
  { title :: T.Text
    , author :: T.Text
    , year :: Int
  } deriving (Generic, Show)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book {author = "Will Kurt", title = "Learn Haskell", year = 2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON
  = "{\"author\": \"Emil Ciroan\", \"title\": \"A Short History of Decay\", \"year\": 1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON
  = "{\"writer\": \"Emil Ciroan\", \"title\": \"A Short History of Decay\", \"year\": 1949}"

bookFromWrongJSON :: Either String Book
bookFromWrongJSON = eitherDecode wrongJSON

--

sampleError :: BC.ByteString
sampleError = "{\"message\": \"oops\", \"error\": 123}"

-- since "error" exists in Haskell, we can't overwrite it with a Record's generated function
data ErrorMessage = ErrorMessage
  { message :: T.Text
  , errorCode :: Int
  } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object ["message" .= message, "error" .= errorCode]

data NOAAResult = NOAAResult
  { uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Double
  , resultId :: T.Text
  } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult
      <$> v
      .:  "uid"
      <*> v
      .:  "mindate"
      <*> v
      .:  "maxdate"
      <*> v
      .:  "name"
      <*> v
      .:  "datacoverage"
      <*> v
      .:  "id"

data Resultset = Resultset
  { offset :: Int
  , count :: Int
  , limit :: Int
  } deriving (Generic, Show)

instance FromJSON Resultset

data Metadata = Metadata { resultset :: Resultset } deriving (Generic, Show)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results :: [NOAAResult]
  } deriving (Generic, Show)

instance FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing        = print "error loading data"
printResults (Just results) = do
  forM_ results $ \result -> do
    let dataName = name result
    print dataName
