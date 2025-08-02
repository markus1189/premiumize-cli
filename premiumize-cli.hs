#!/usr/bin/env nix
#! nix shell --impure --expr ``with import <nixpkgs>{}; pkgs.haskellPackages.ghcWithPackages (ps: with ps; [wreq aeson optparse-applicative text bytestring lens])`` --command runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

import Network.Wreq (get, post, defaults, param, responseBody)
import qualified Network.Wreq as W
import Control.Lens
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as L8
import Options.Applicative
import qualified Options.Applicative as OA
import Data.List (intercalate)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import GHC.Generics
import Control.Exception (try, SomeException)
import Prelude hiding (id)

-- Base URL for Premiumize.me API
baseURL :: String
baseURL = "https://www.premiumize.me/api"

-- Data Types for API Responses
data ApiResponse a = ApiResponse
  { status :: Text
  , message :: Maybe Text
  , result :: Maybe a
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (ApiResponse a)

data AccountInfo = AccountInfo
  { customer_id :: Text
  , limit_used :: Double
  , space_used :: Double
  , premium_until :: Maybe Text
  , username :: Text
  } deriving (Generic, Show)

instance FromJSON AccountInfo
instance ToJSON AccountInfo

data FolderItem = FolderItem
  { id :: Text
  , name :: Text
  , type_ :: Text
  , size :: Maybe Integer
  , created_at :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON FolderItem where
  parseJSON = withObject "FolderItem" $ \o -> FolderItem
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "type"
    <*> o .:? "size"
    <*> o .:? "created_at"

instance ToJSON FolderItem where
  toJSON FolderItem{..} = object
    [ "id" A..= id
    , "name" A..= name
    , "type" A..= type_
    , "size" A..= size
    , "created_at" A..= created_at
    ]

data Transfer = Transfer
  { transfer_id :: Text
  , transfer_name :: Text
  , transfer_status :: Text
  , transfer_progress :: Double
  , transfer_src :: Text
  , transfer_folder_id :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON Transfer where
  parseJSON = withObject "Transfer" $ \o -> Transfer
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "status"
    <*> o .: "progress"
    <*> o .: "src"
    <*> o .:? "folder_id"

instance ToJSON Transfer where
  toJSON Transfer{..} = object
    [ "id" A..= transfer_id
    , "name" A..= transfer_name
    , "status" A..= transfer_status
    , "progress" A..= transfer_progress
    , "src" A..= transfer_src
    , "folder_id" A..= transfer_folder_id
    ]

data CacheResult = CacheResult
  { filename :: Text
  , filesize :: Integer
  , cached :: Bool
  } deriving (Generic, Show)

instance FromJSON CacheResult
instance ToJSON CacheResult

data TransferListResponse = TransferListResponse
  { tl_status :: Text
  , tl_transfers :: [Transfer]
  } deriving (Generic, Show)

instance FromJSON TransferListResponse where
  parseJSON = withObject "TransferListResponse" $ \o -> TransferListResponse
    <$> o .: "status"
    <*> o .: "transfers"

-- CLI Data Types
data GlobalOptions = GlobalOptions
  { optJson :: Bool
  , optVerbose :: Bool
  } deriving Show

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
  deriving Show

-- HTTP Client Functions
makeAuthenticatedRequest :: String -> W.Options -> IO (W.Response L8.ByteString)
makeAuthenticatedRequest url opts = do
  apiKey <- lookupEnv "PREMIUMIZE_API_KEY"
  case apiKey of
    Nothing -> do
      putStrLn "Error: PREMIUMIZE_API_KEY environment variable not set"
      exitFailure
    Just key -> do
      let authOpts = opts & param "apikey" .~ [T.pack key]
      W.getWith authOpts (baseURL ++ url)

makeAuthenticatedPost :: String -> [W.FormParam] -> IO (W.Response L8.ByteString)
makeAuthenticatedPost url params = do
  apiKey <- lookupEnv "PREMIUMIZE_API_KEY"
  case apiKey of
    Nothing -> do
      putStrLn "Error: PREMIUMIZE_API_KEY environment variable not set"
      exitFailure
    Just key -> do
      let authParams = ("apikey" W.:= T.pack key) : params
      post (baseURL ++ url) authParams

-- API Implementation Functions
getAccountInfo :: GlobalOptions -> IO ()
getAccountInfo GlobalOptions{..} = do
  result <- try $ makeAuthenticatedRequest "/account/info" defaults
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " ++ err)
        Right (ApiResponse status msg (Just info :: Maybe AccountInfo)) ->
          if optJson
            then L8.putStrLn $ encode info
            else printAccountInfo info
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

printAccountInfo :: AccountInfo -> IO ()
printAccountInfo AccountInfo{..} = do
  putStrLn $ "Customer ID: " ++ T.unpack customer_id
  putStrLn $ "Username: " ++ T.unpack username
  putStrLn $ "Limit Used: " ++ show limit_used ++ "%"
  putStrLn $ "Space Used: " ++ show space_used ++ " bytes"
  case premium_until of
    Just until -> putStrLn $ "Premium Until: " ++ T.unpack until
    Nothing -> putStrLn "Premium Until: N/A"

listFolder :: GlobalOptions -> Maybe Text -> IO ()
listFolder GlobalOptions{..} folderId = do
  let opts = case folderId of
        Just fid -> defaults & param "id" .~ [fid]
        Nothing -> defaults
  result <- try $ makeAuthenticatedRequest "/folder/list" opts
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " ++ err)
        Right (ApiResponse status msg (Just items :: Maybe [FolderItem])) ->
          if optJson
            then L8.putStrLn $ encode items
            else mapM_ printFolderItem items
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

printFolderItem :: FolderItem -> IO ()
printFolderItem FolderItem{..} = do
  putStrLn $ T.unpack id ++ " | " ++ T.unpack name ++ " | " ++ T.unpack type_ ++ 
    maybe "" ((" | " ++) . show) size

createFolder :: GlobalOptions -> Text -> Maybe Text -> IO ()
createFolder GlobalOptions{..} folderName parentId = do
  let params = ("name" W.:= folderName) : 
               case parentId of
                 Just pid -> [("parent_id" W.:= pid)]
                 Nothing -> []
  result <- try $ makeAuthenticatedPost "/folder/create" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Folder created successfully"
              else handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

renameFolder :: GlobalOptions -> Text -> Text -> IO ()
renameFolder GlobalOptions{..} folderId newName = do
  let params = [("id" W.:= folderId), ("name" W.:= newName)]
  result <- try $ makeAuthenticatedPost "/folder/rename" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Folder renamed successfully"
              else handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

deleteFolder :: GlobalOptions -> Text -> IO ()
deleteFolder GlobalOptions{..} folderId = do
  let params = [("id" W.:= folderId)]
  result <- try $ makeAuthenticatedPost "/folder/delete" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Folder deleted successfully"
              else handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

searchFolder :: GlobalOptions -> Text -> IO ()
searchFolder GlobalOptions{..} query = do
  let opts = defaults & param "q" .~ [query]
  result <- try $ makeAuthenticatedRequest "/folder/search" opts
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " ++ err)
        Right (ApiResponse status msg (Just items :: Maybe [FolderItem])) ->
          if optJson
            then L8.putStrLn $ encode items
            else mapM_ printFolderItem items
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

listAllItems :: GlobalOptions -> IO ()
listAllItems GlobalOptions{..} = do
  result <- try $ makeAuthenticatedRequest "/item/listall" defaults
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " ++ err)
        Right (ApiResponse status msg (Just items :: Maybe [FolderItem])) ->
          if optJson
            then L8.putStrLn $ encode items
            else mapM_ printFolderItem items
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

deleteItem :: GlobalOptions -> Text -> IO ()
deleteItem GlobalOptions{..} itemId = do
  let params = [("id" W.:= itemId)]
  result <- try $ makeAuthenticatedPost "/item/delete" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Item deleted successfully"
              else handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

renameItem :: GlobalOptions -> Text -> Text -> IO ()
renameItem GlobalOptions{..} itemId newName = do
  let params = [("id" W.:= itemId), ("name" W.:= newName)]
  result <- try $ makeAuthenticatedPost "/item/rename" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Item renamed successfully"
              else handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

getItemDetails :: GlobalOptions -> Text -> IO ()
getItemDetails GlobalOptions{..} itemId = do
  let opts = defaults & param "id" .~ [itemId]
  result <- try $ makeAuthenticatedRequest "/item/details" opts
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " ++ err)
        Right (ApiResponse status msg (Just item :: Maybe FolderItem)) ->
          if optJson
            then L8.putStrLn $ encode item
            else printFolderItem item
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

createTransfer :: GlobalOptions -> Text -> Maybe Text -> IO ()
createTransfer GlobalOptions{..} srcUrl folderId = do
  let params = ("src" W.:= srcUrl) : 
               case folderId of
                 Just fid -> [("folder_id" W.:= fid)]
                 Nothing -> []
  result <- try $ makeAuthenticatedPost "/transfer/create" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Transfer created successfully"
              else handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

createDirectDownload :: GlobalOptions -> Text -> IO ()
createDirectDownload GlobalOptions{..} srcUrl = do
  let params = [("src" W.:= srcUrl)]
  result <- try $ makeAuthenticatedPost "/transfer/directdl" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (Just url :: Maybe Text)) ->
            putStrLn $ "Direct download URL: " ++ T.unpack url
          Right (ApiResponse status msg Nothing) ->
            handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

listTransfers :: GlobalOptions -> IO ()
listTransfers GlobalOptions{..} = do
  result <- try $ makeAuthenticatedRequest "/transfer/list" defaults
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " ++ err)
        Right (TransferListResponse status transfers) ->
          if status == "success"
            then if optJson
              then L8.putStrLn $ encode transfers
              else mapM_ printTransfer transfers
            else handleError optJson ("API error: " ++ T.unpack status)

printTransfer :: Transfer -> IO ()
printTransfer Transfer{..} = do
  putStrLn $ T.unpack transfer_id ++ " | " ++ T.unpack transfer_name ++ " | " ++ T.unpack transfer_status ++ 
    " | " ++ show transfer_progress ++ "% | " ++ T.unpack transfer_src

deleteTransfer :: GlobalOptions -> Text -> IO ()
deleteTransfer GlobalOptions{..} transferId = do
  let params = [("id" W.:= transferId)]
  result <- try $ makeAuthenticatedPost "/transfer/delete" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Transfer deleted successfully"
              else handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

listServices :: GlobalOptions -> IO ()
listServices GlobalOptions{..} = do
  result <- try $ makeAuthenticatedRequest "/services/list" defaults
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " ++ err)
          Right (ApiResponse status msg (Just services :: Maybe [Text])) ->
            mapM_ (putStrLn . T.unpack) services
          Right (ApiResponse status msg Nothing) ->
            handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

checkCache :: GlobalOptions -> [Text] -> IO ()
checkCache GlobalOptions{..} items = do
  let opts = defaults & param "items" .~ [T.intercalate "," items]
  result <- try $ makeAuthenticatedRequest "/cache/check" opts
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " ++ err)
        Right (ApiResponse status msg (Just results :: Maybe [CacheResult])) ->
          if optJson
            then L8.putStrLn $ encode results
            else mapM_ printCacheResult results
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " ++ T.unpack status ++ maybe "" ((" - " ++) . T.unpack) msg)

printCacheResult :: CacheResult -> IO ()
printCacheResult CacheResult{..} = do
  putStrLn $ T.unpack filename ++ " | " ++ show filesize ++ " bytes | " ++ 
    (if cached then "CACHED" else "NOT CACHED")

handleError :: Bool -> String -> IO ()
handleError jsonOutput msg = do
  if jsonOutput
    then L8.putStrLn $ encode $ object ["error" A..= msg]
    else putStrLn $ "Error: " ++ msg
  exitFailure

-- Help text and descriptions
versionText :: String
versionText = "premiumize-cli 1.0.0"

headerText :: String
headerText = "Premiumize.me API CLI tool - manage downloads, transfers and cloud storage"

footerText :: String
footerText = unlines
  [ ""
  , "Environment Variables:"
  , "  PREMIUMIZE_API_KEY    Your Premiumize.me API key (required)"
  , ""
  , "Examples:"
  , "  premiumize-cli account info"
  , "  premiumize-cli transfer list"
  , "  premiumize-cli transfer create --src 'magnet:?xt=...'"
  , "  premiumize-cli folder list --id 'folder123'"
  , ""
  , "For more information, visit: https://www.premiumize.me/api"
  ]

-- CLI Parser
globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions
  <$> switch 
      ( long "json" 
      <> short 'j'
      <> help "Output results in JSON format" )
  <*> switch 
      ( long "verbose" 
      <> short 'v' 
      <> help "Enable verbose output and debugging information" )

commandParser :: Parser Command
commandParser = subparser
  ( command "account" 
      (info (accountCommands <**> helper) 
        (progDesc "Account information and settings" <> fullDesc))
 <> command "folder" 
      (info (folderCommands <**> helper) 
        (progDesc "Manage folders and directory structure" <> fullDesc))
 <> command "item" 
      (info (itemCommands <**> helper) 
        (progDesc "Manage individual files and items" <> fullDesc))
 <> command "transfer" 
      (info (transferCommands <**> helper) 
        (progDesc "Download management and transfer operations" <> fullDesc))
 <> command "cache" 
      (info (cacheCommands <**> helper) 
        (progDesc "Check cache availability for files" <> fullDesc))
 <> command "services" 
      (info (servicesCommands <**> helper) 
        (progDesc "List supported download services" <> fullDesc))
  )

accountCommands :: Parser Command
accountCommands = subparser
  ( command "info" 
      (info (pure CmdAccountInfo <**> helper) 
        (progDesc "Display account information including usage, limits and premium status"
         <> footer "Example:\n  account info"))
  )

folderCommands :: Parser Command
folderCommands = subparser
  ( command "list" 
      (info (folderListCmd <**> helper) 
        (progDesc "List contents of a folder (or root if no ID specified)"
         <> footer "Examples:\n  folder list              # List root folder\n  folder list --id abc123  # List specific folder"))
 <> command "create" 
      (info (folderCreateCmd <**> helper) 
        (progDesc "Create a new folder, optionally inside a parent folder"
         <> footer "Examples:\n  folder create --name 'My Folder'\n  folder create --name 'Subfolder' --parent-id abc123"))
 <> command "rename" 
      (info (folderRenameCmd <**> helper) 
        (progDesc "Rename an existing folder"
         <> footer "Example:\n  folder rename --id abc123 --name 'New Name'"))
 <> command "delete" 
      (info (folderDeleteCmd <**> helper) 
        (progDesc "Delete a folder and all its contents"
         <> footer "Example:\n  folder delete --id abc123"))
 <> command "search" 
      (info (folderSearchCmd <**> helper) 
        (progDesc "Search for folders and files by name or content"
         <> footer "Example:\n  folder search --query 'movie'"))
  )

folderListCmd :: Parser Command
folderListCmd = CmdFolderList <$> optional 
  (strOption (long "id" <> short 'i' <> help "Folder ID to list contents of"))

folderCreateCmd :: Parser Command
folderCreateCmd = CmdFolderCreate
  <$> strOption 
      (long "name" <> short 'n' <> help "Name for the new folder")
  <*> optional 
      (strOption (long "parent-id" <> short 'p' <> help "Parent folder ID (creates in root if not specified)"))

folderRenameCmd :: Parser Command
folderRenameCmd = CmdFolderRename
  <$> strOption 
      (long "id" <> short 'i' <> help "ID of the folder to rename")
  <*> strOption 
      (long "name" <> short 'n' <> help "New name for the folder")

folderDeleteCmd :: Parser Command
folderDeleteCmd = CmdFolderDelete <$> strOption 
  (long "id" <> short 'i' <> help "ID of the folder to delete")

folderSearchCmd :: Parser Command
folderSearchCmd = CmdFolderSearch <$> strOption 
  (long "query" <> short 'q' <> help "Search terms to find folders and files")

itemCommands :: Parser Command
itemCommands = subparser
  ( command "listall" 
      (info (pure CmdItemListAll <**> helper) 
        (progDesc "List all files and folders in your account"
         <> footer "Example:\n  item listall"))
 <> command "delete" 
      (info (itemDeleteCmd <**> helper) 
        (progDesc "Delete a specific file or folder"
         <> footer "Example:\n  item delete --id abc123"))
 <> command "rename" 
      (info (itemRenameCmd <**> helper) 
        (progDesc "Rename a file or folder"
         <> footer "Example:\n  item rename --id abc123 --name 'New Name'"))
 <> command "details" 
      (info (itemDetailsCmd <**> helper) 
        (progDesc "Show detailed information about a specific item"
         <> footer "Example:\n  item details --id abc123"))
  )

itemDeleteCmd :: Parser Command
itemDeleteCmd = CmdItemDelete <$> strOption 
  (long "id" <> short 'i' <> help "ID of the item to delete")

itemRenameCmd :: Parser Command
itemRenameCmd = CmdItemRename
  <$> strOption 
      (long "id" <> short 'i' <> help "ID of the item to rename")
  <*> strOption 
      (long "name" <> short 'n' <> help "New name for the item")

itemDetailsCmd :: Parser Command
itemDetailsCmd = CmdItemDetails <$> strOption 
  (long "id" <> short 'i' <> help "ID of the item to show details for")

transferCommands :: Parser Command
transferCommands = subparser
  ( command "create" 
      (info (transferCreateCmd <**> helper) 
        (progDesc "Start downloading from a URL (magnet, torrent, or direct link)"
         <> footer "Examples:\n  transfer create --src 'magnet:?xt=urn:btih:...'\n  transfer create --src 'https://example.com/file.zip' --folder-id abc123"))
 <> command "directdl" 
      (info (transferDirectDLCmd <**> helper) 
        (progDesc "Generate a direct download link without storing the file"
         <> footer "Example:\n  transfer directdl --src 'https://example.com/file.zip'"))
 <> command "list" 
      (info (pure CmdTransferList <**> helper) 
        (progDesc "Show all downloads with their status and progress"
         <> footer "Shows: ID | Name | Status | Progress | Source URL"))
 <> command "delete" 
      (info (transferDeleteCmd <**> helper) 
        (progDesc "Cancel and remove a download"
         <> footer "Example:\n  transfer delete --id abc123"))
  )

transferCreateCmd :: Parser Command
transferCreateCmd = CmdTransferCreate
  <$> strOption 
      (long "src" <> short 's' <> help "Source URL (magnet link, torrent URL, or direct download link)")
  <*> optional 
      (strOption (long "folder-id" <> short 'f' <> help "Destination folder ID (saves to root if not specified)"))

transferDirectDLCmd :: Parser Command
transferDirectDLCmd = CmdTransferDirectDL <$> strOption 
  (long "src" <> short 's' <> help "Source URL to generate direct download link for")

transferDeleteCmd :: Parser Command
transferDeleteCmd = CmdTransferDelete <$> strOption 
  (long "id" <> short 'i' <> help "ID of the transfer to cancel and delete")

cacheCommands :: Parser Command
cacheCommands = subparser
  ( command "check" 
      (info (cacheCheckCmd <**> helper) 
        (progDesc "Check if files are available in the Premiumize cache"
         <> footer "Example:\n  cache check --item 'filename.zip' --item 'otherhash'"))
  )

cacheCheckCmd :: Parser Command
cacheCheckCmd = CmdCacheCheck <$> some 
  (strOption (long "item" <> short 'i' <> help "Hash or filename to check (can be used multiple times)"))

servicesCommands :: Parser Command
servicesCommands = subparser
  ( command "list" 
      (info (pure CmdServicesList <**> helper) 
        (progDesc "Show all supported download services and file hosters"
         <> footer "Example:\n  services list"))
  )

-- Main execution
runCommand :: GlobalOptions -> Command -> IO ()
runCommand opts cmd = case cmd of
  CmdAccountInfo -> getAccountInfo opts
  CmdFolderList folderId -> listFolder opts folderId
  CmdFolderCreate name parentId -> createFolder opts name parentId
  CmdFolderRename folderId newName -> renameFolder opts folderId newName
  CmdFolderDelete folderId -> deleteFolder opts folderId
  CmdFolderSearch query -> searchFolder opts query
  CmdItemListAll -> listAllItems opts
  CmdItemDelete itemId -> deleteItem opts itemId
  CmdItemRename itemId newName -> renameItem opts itemId newName
  CmdItemDetails itemId -> getItemDetails opts itemId
  CmdTransferCreate srcUrl folderId -> createTransfer opts srcUrl folderId
  CmdTransferDirectDL srcUrl -> createDirectDownload opts srcUrl
  CmdTransferList -> listTransfers opts
  CmdTransferDelete transferId -> deleteTransfer opts transferId
  CmdCacheCheck items -> checkCache opts items
  CmdServicesList -> listServices opts

main :: IO ()
main = do
  (opts, cmd) <- execParser $
    info ((,) <$> globalOptionsParser <*> commandParser <**> helper)
      ( fullDesc
     <> progDesc headerText
     <> OA.header versionText
     <> footer (footerText)
      )
  runCommand opts cmd