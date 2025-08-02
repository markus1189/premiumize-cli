{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PremiumizeCli.API
  ( getAccountInfo,
    listFolder,
    createFolder,
    renameFolder,
    deleteFolder,
    searchFolder,
    listAllItems,
    deleteItem,
    renameItem,
    getItemDetails,
    createTransfer,
    createDirectDownload,
    listTransfers,
    deleteTransfer,
    listServices,
    checkCache,
  )
where

import Control.Exception (SomeException, try)
import Control.Lens
import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wreq (defaults, get, param, post, responseBody)
import qualified Network.Wreq as W
import PremiumizeCli.Types
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

-- Base URL for Premiumize.me API
baseURL :: String
baseURL = "https://www.premiumize.me/api"

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
      W.getWith authOpts (baseURL <> url)

makeAuthenticatedPost :: String -> [W.FormParam] -> IO (W.Response L8.ByteString)
makeAuthenticatedPost url params = do
  apiKey <- lookupEnv "PREMIUMIZE_API_KEY"
  case apiKey of
    Nothing -> do
      putStrLn "Error: PREMIUMIZE_API_KEY environment variable not set"
      exitFailure
    Just key -> do
      let authParams = ("apikey" W.:= T.pack key) : params
      post (baseURL <> url) authParams

-- Utility Functions
handleError :: Bool -> String -> IO ()
handleError jsonOutput msg = do
  if jsonOutput
    then L8.putStrLn . encode $ object ["error" A..= msg]
    else putStrLn $ "Error: " <> msg
  exitFailure

printAccountInfo :: AccountInfo -> IO ()
printAccountInfo AccountInfo {..} = do
  putStrLn $ "Customer ID: " <> T.unpack customer_id
  putStrLn $ "Username: " <> T.unpack username
  putStrLn $ "Limit Used: " <> (show limit_used <> "%")
  putStrLn $ "Space Used: " <> (show space_used <> " bytes")
  case premium_until of
    Just until -> putStrLn $ "Premium Until: " <> T.unpack until
    Nothing -> putStrLn "Premium Until: N/A"

printFolderItem :: FolderItem -> IO ()
printFolderItem FolderItem {..} = do
  putStrLn $ T.unpack id <> (" | " <> (T.unpack name <> (" | " <> (T.unpack type_ <> maybe "" ((" | " <>) . show) size))))

printTransfer :: Transfer -> IO ()
printTransfer Transfer {..} = do
  putStrLn $ T.unpack transfer_id <> (" | " <> (T.unpack transfer_name <> (" | " <> (T.unpack transfer_status <> (" | " <> (show transfer_progress <> ("% | " <> T.unpack transfer_src)))))))

printCacheResult :: CacheResult -> IO ()
printCacheResult CacheResult {..} = do
  putStrLn $ T.unpack filename <> (" | " <> (show filesize <> (" bytes | " <> (if cached then "CACHED" else "NOT CACHED"))))

-- API Implementation Functions
getAccountInfo :: GlobalOptions -> IO ()
getAccountInfo GlobalOptions {..} = do
  result <- try $ makeAuthenticatedRequest "/account/info" defaults
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " <> err)
        Right (ApiResponse status msg (Just info :: Maybe AccountInfo)) ->
          if optJson
            then L8.putStrLn $ encode info
            else printAccountInfo info
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

listFolder :: GlobalOptions -> Maybe Text -> IO ()
listFolder GlobalOptions {..} folderId = do
  let opts = case folderId of
        Just fid -> defaults & param "id" .~ [fid]
        Nothing -> defaults
  result <- try $ makeAuthenticatedRequest "/folder/list" opts
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " <> err)
        Right (ApiResponse status msg (Just items :: Maybe [FolderItem])) ->
          if optJson
            then L8.putStrLn $ encode items
            else mapM_ printFolderItem items
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

createFolder :: GlobalOptions -> Text -> Maybe Text -> IO ()
createFolder GlobalOptions {..} folderName parentId = do
  let params =
        ("name" W.:= folderName)
          : case parentId of
            Just pid -> ["parent_id" W.:= pid]
            Nothing -> []
  result <- try $ makeAuthenticatedPost "/folder/create" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Folder created successfully"
              else handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

renameFolder :: GlobalOptions -> Text -> Text -> IO ()
renameFolder GlobalOptions {..} folderId newName = do
  let params = ["id" W.:= folderId, "name" W.:= newName]
  result <- try $ makeAuthenticatedPost "/folder/rename" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Folder renamed successfully"
              else handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

deleteFolder :: GlobalOptions -> Text -> IO ()
deleteFolder GlobalOptions {..} folderId = do
  let params = ["id" W.:= folderId]
  result <- try $ makeAuthenticatedPost "/folder/delete" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Folder deleted successfully"
              else handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

searchFolder :: GlobalOptions -> Text -> IO ()
searchFolder GlobalOptions {..} query = do
  let opts = defaults & param "q" .~ [query]
  result <- try $ makeAuthenticatedRequest "/folder/search" opts
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " <> err)
        Right (ApiResponse status msg (Just items :: Maybe [FolderItem])) ->
          if optJson
            then L8.putStrLn $ encode items
            else mapM_ printFolderItem items
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

listAllItems :: GlobalOptions -> IO ()
listAllItems GlobalOptions {..} = do
  result <- try $ makeAuthenticatedRequest "/item/listall" defaults
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " <> err)
        Right (ApiResponse status msg (Just items :: Maybe [FolderItem])) ->
          if optJson
            then L8.putStrLn $ encode items
            else mapM_ printFolderItem items
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

deleteItem :: GlobalOptions -> Text -> IO ()
deleteItem GlobalOptions {..} itemId = do
  let params = ["id" W.:= itemId]
  result <- try $ makeAuthenticatedPost "/item/delete" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Item deleted successfully"
              else handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

renameItem :: GlobalOptions -> Text -> Text -> IO ()
renameItem GlobalOptions {..} itemId newName = do
  let params = ["id" W.:= itemId, "name" W.:= newName]
  result <- try $ makeAuthenticatedPost "/item/rename" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Item renamed successfully"
              else handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

getItemDetails :: GlobalOptions -> Text -> IO ()
getItemDetails GlobalOptions {..} itemId = do
  let opts = defaults & param "id" .~ [itemId]
  result <- try $ makeAuthenticatedRequest "/item/details" opts
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " <> err)
        Right (ApiResponse status msg (Just item :: Maybe FolderItem)) ->
          if optJson
            then L8.putStrLn $ encode item
            else printFolderItem item
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

createTransfer :: GlobalOptions -> Text -> Maybe Text -> IO ()
createTransfer GlobalOptions {..} srcUrl folderId = do
  let params =
        ("src" W.:= srcUrl)
          : case folderId of
            Just fid -> ["folder_id" W.:= fid]
            Nothing -> []
  result <- try $ makeAuthenticatedPost "/transfer/create" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Transfer created successfully"
              else handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

createDirectDownload :: GlobalOptions -> Text -> IO ()
createDirectDownload GlobalOptions {..} srcUrl = do
  let params = ["src" W.:= srcUrl]
  result <- try $ makeAuthenticatedPost "/transfer/directdl" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (Just url :: Maybe Text)) ->
            putStrLn $ "Direct download URL: " <> T.unpack url
          Right (ApiResponse status msg Nothing) ->
            handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

listTransfers :: GlobalOptions -> IO ()
listTransfers GlobalOptions {..} = do
  result <- try $ makeAuthenticatedRequest "/transfer/list" defaults
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " <> err)
        Right (TransferListResponse status transfers) ->
          if status == "success"
            then
              if optJson
                then L8.putStrLn $ encode transfers
                else mapM_ printTransfer transfers
            else handleError optJson ("API error: " <> T.unpack status)

deleteTransfer :: GlobalOptions -> Text -> IO ()
deleteTransfer GlobalOptions {..} transferId = do
  let params = ["id" W.:= transferId]
  result <- try $ makeAuthenticatedPost "/transfer/delete" params
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (_ :: Maybe Value)) ->
            if status == "success"
              then putStrLn "Transfer deleted successfully"
              else handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

listServices :: GlobalOptions -> IO ()
listServices GlobalOptions {..} = do
  result <- try $ makeAuthenticatedRequest "/services/list" defaults
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      if optJson
        then L8.putStrLn body
        else case eitherDecode body of
          Left err -> handleError optJson ("JSON parse error: " <> err)
          Right (ApiResponse status msg (Just services :: Maybe [Text])) ->
            mapM_ (putStrLn . T.unpack) services
          Right (ApiResponse status msg Nothing) ->
            handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))

checkCache :: GlobalOptions -> [Text] -> IO ()
checkCache GlobalOptions {..} items = do
  let opts = defaults & param "items" .~ [T.intercalate "," items]
  result <- try $ makeAuthenticatedRequest "/cache/check" opts
  case result of
    Left e -> handleError optJson (show (e :: SomeException))
    Right r -> do
      let body = r ^. responseBody
      case eitherDecode body of
        Left err -> handleError optJson ("JSON parse error: " <> err)
        Right (ApiResponse status msg (Just results :: Maybe [CacheResult])) ->
          if optJson
            then L8.putStrLn $ encode results
            else mapM_ printCacheResult results
        Right (ApiResponse status msg Nothing) ->
          handleError optJson ("API error: " <> (T.unpack status <> maybe "" ((" - " <>) . T.unpack) msg))
