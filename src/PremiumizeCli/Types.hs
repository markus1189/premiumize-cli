{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PremiumizeCli.Types
  ( ApiResponse (..),
    AccountInfo (..),
    FolderItem (..),
    Transfer (..),
    CacheResult (..),
    TransferListResponse (..),
    GlobalOptions (..),
    Command (..),
  )
where

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (id)

-- Data Types for API Responses
data ApiResponse a = ApiResponse
  { status :: Text,
    message :: Maybe Text,
    result :: Maybe a
  }
  deriving (Generic, Show)

instance (FromJSON a) => FromJSON (ApiResponse a)

data AccountInfo = AccountInfo
  { customer_id :: Text,
    limit_used :: Double,
    space_used :: Double,
    premium_until :: Maybe Text,
    username :: Text
  }
  deriving (Generic, Show)

instance FromJSON AccountInfo

instance ToJSON AccountInfo

data FolderItem = FolderItem
  { id :: Text,
    name :: Text,
    type_ :: Text,
    size :: Maybe Integer,
    created_at :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON FolderItem where
  parseJSON = withObject "FolderItem" $ \o ->
    FolderItem
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "type"
      <*> o .:? "size"
      <*> o .:? "created_at"

instance ToJSON FolderItem where
  toJSON FolderItem {..} =
    object
      [ "id" A..= id,
        "name" A..= name,
        "type" A..= type_,
        "size" A..= size,
        "created_at" A..= created_at
      ]

data Transfer = Transfer
  { transfer_id :: Text,
    transfer_name :: Text,
    transfer_status :: Text,
    transfer_progress :: Double,
    transfer_src :: Text,
    transfer_folder_id :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Transfer where
  parseJSON = withObject "Transfer" $ \o ->
    Transfer
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "status"
      <*> o .: "progress"
      <*> o .: "src"
      <*> o .:? "folder_id"

instance ToJSON Transfer where
  toJSON Transfer {..} =
    object
      [ "id" A..= transfer_id,
        "name" A..= transfer_name,
        "status" A..= transfer_status,
        "progress" A..= transfer_progress,
        "src" A..= transfer_src,
        "folder_id" A..= transfer_folder_id
      ]

data CacheResult = CacheResult
  { filename :: Text,
    filesize :: Integer,
    cached :: Bool
  }
  deriving (Generic, Show)

instance FromJSON CacheResult

instance ToJSON CacheResult

data TransferListResponse = TransferListResponse
  { tl_status :: Text,
    tl_transfers :: [Transfer]
  }
  deriving (Generic, Show)

instance FromJSON TransferListResponse where
  parseJSON = withObject "TransferListResponse" $ \o ->
    TransferListResponse
      <$> o .: "status"
      <*> o .: "transfers"

-- CLI Data Types
data GlobalOptions = GlobalOptions
  { optJson :: Bool,
    optVerbose :: Bool
  }
  deriving (Show)

data Command
  = CmdAccountInfo
  | CmdFolderList (Maybe Text)
  | CmdFolderCreate Text (Maybe Text)
  | CmdFolderRename Text Text
  | CmdFolderDelete Text
  | CmdFolderSearch Text
  | CmdItemListAll
  | CmdItemDelete Text
  | CmdItemRename Text Text
  | CmdItemDetails Text
  | CmdTransferCreate Text (Maybe Text)
  | CmdTransferDirectDL Text
  | CmdTransferList
  | CmdTransferDelete Text
  | CmdCacheCheck [Text]
  | CmdServicesList
  deriving (Show)
