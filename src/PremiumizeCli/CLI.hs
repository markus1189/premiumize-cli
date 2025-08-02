module PremiumizeCli.CLI
  ( parseOptions,
    runCommand,
  )
where

import Data.Text (Text)
import Options.Applicative
import qualified Options.Applicative as OA
import PremiumizeCli.API
import PremiumizeCli.Types

-- Help text and descriptions
versionText :: String
versionText = "premiumize-cli 1.0.0"

headerText :: String
headerText = "Premiumize.me API CLI tool - manage downloads, transfers and cloud storage"

footerText :: String
footerText =
  unlines
    [ "",
      "Environment Variables:",
      "  PREMIUMIZE_API_KEY    Your Premiumize.me API key (required)",
      "",
      "Examples:",
      "  premiumize-cli account info",
      "  premiumize-cli transfer list",
      "  premiumize-cli transfer create --src 'magnet:?xt=...'",
      "  premiumize-cli folder list --id 'folder123'",
      "",
      "For more information, visit: https://www.premiumize.me/api"
    ]

-- CLI Parser
globalOptionsParser :: Parser GlobalOptions
globalOptionsParser =
  GlobalOptions
    <$> switch
      ( long "json"
          <> short 'j'
          <> help "Output results in JSON format"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Enable verbose output and debugging information"
      )

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "account"
        ( info
            (accountCommands <**> helper)
            (progDesc "Account information and settings" <> fullDesc)
        )
        <> command
          "transfer"
          ( info
              (transferCommands <**> helper)
              (progDesc "Download management and transfer operations" <> fullDesc)
          )
        <> command
          "folder"
          ( info
              (folderCommands <**> helper)
              (progDesc "Manage folders and directory structure" <> fullDesc)
          )
        <> command
          "item"
          ( info
              (itemCommands <**> helper)
              (progDesc "Manage individual files and items" <> fullDesc)
          )
        <> command
          "cache"
          ( info
              (cacheCommands <**> helper)
              (progDesc "Check cache availability for files" <> fullDesc)
          )
        <> command
          "services"
          ( info
              (servicesCommands <**> helper)
              (progDesc "List supported download services" <> fullDesc)
          )
    )

accountCommands :: Parser Command
accountCommands =
  subparser
    ( command
        "info"
        ( info
            (pure CmdAccountInfo <**> helper)
            ( progDesc "Display account information including usage, limits and premium status"
                <> footer "Example:\n  account info"
            )
        )
    )

folderCommands :: Parser Command
folderCommands =
  subparser
    ( command
        "list"
        ( info
            (folderListCmd <**> helper)
            ( progDesc "List contents of a folder (or root if no ID specified)"
                <> footer "Examples:\n  folder list              # List root folder\n  folder list --id abc123  # List specific folder"
            )
        )
        <> command
          "create"
          ( info
              (folderCreateCmd <**> helper)
              ( progDesc "Create a new folder, optionally inside a parent folder"
                  <> footer "Examples:\n  folder create --name 'My Folder'\n  folder create --name 'Subfolder' --parent-id abc123"
              )
          )
        <> command
          "rename"
          ( info
              (folderRenameCmd <**> helper)
              ( progDesc "Rename an existing folder"
                  <> footer "Example:\n  folder rename --id abc123 --name 'New Name'"
              )
          )
        <> command
          "delete"
          ( info
              (folderDeleteCmd <**> helper)
              ( progDesc "Delete a folder and all its contents"
                  <> footer "Example:\n  folder delete --id abc123"
              )
          )
        <> command
          "search"
          ( info
              (folderSearchCmd <**> helper)
              ( progDesc "Search for folders and files by name or content"
                  <> footer "Example:\n  folder search --query 'movie'"
              )
          )
    )

folderListCmd :: Parser Command
folderListCmd =
  CmdFolderList
    <$> optional
      (strOption (long "id" <> short 'i' <> help "Folder ID to list contents of"))

folderCreateCmd :: Parser Command
folderCreateCmd =
  CmdFolderCreate
    <$> strOption
      (long "name" <> short 'n' <> help "Name for the new folder")
    <*> optional
      (strOption (long "parent-id" <> short 'p' <> help "Parent folder ID (creates in root if not specified)"))

folderRenameCmd :: Parser Command
folderRenameCmd =
  CmdFolderRename
    <$> strOption
      (long "id" <> short 'i' <> help "ID of the folder to rename")
    <*> strOption
      (long "name" <> short 'n' <> help "New name for the folder")

folderDeleteCmd :: Parser Command
folderDeleteCmd =
  CmdFolderDelete
    <$> strOption
      (long "id" <> short 'i' <> help "ID of the folder to delete")

folderSearchCmd :: Parser Command
folderSearchCmd =
  CmdFolderSearch
    <$> strOption
      (long "query" <> short 'q' <> help "Search terms to find folders and files")

itemCommands :: Parser Command
itemCommands =
  subparser
    ( command
        "listall"
        ( info
            (pure CmdItemListAll <**> helper)
            ( progDesc "List all files and folders in your account"
                <> footer "Example:\n  item listall"
            )
        )
        <> command
          "delete"
          ( info
              (itemDeleteCmd <**> helper)
              ( progDesc "Delete a specific file or folder"
                  <> footer "Example:\n  item delete --id abc123"
              )
          )
        <> command
          "rename"
          ( info
              (itemRenameCmd <**> helper)
              ( progDesc "Rename a file or folder"
                  <> footer "Example:\n  item rename --id abc123 --name 'New Name'"
              )
          )
        <> command
          "details"
          ( info
              (itemDetailsCmd <**> helper)
              ( progDesc "Show detailed information about a specific item"
                  <> footer "Example:\n  item details --id abc123"
              )
          )
    )

itemDeleteCmd :: Parser Command
itemDeleteCmd =
  CmdItemDelete
    <$> strOption
      (long "id" <> short 'i' <> help "ID of the item to delete")

itemRenameCmd :: Parser Command
itemRenameCmd =
  CmdItemRename
    <$> strOption
      (long "id" <> short 'i' <> help "ID of the item to rename")
    <*> strOption
      (long "name" <> short 'n' <> help "New name for the item")

itemDetailsCmd :: Parser Command
itemDetailsCmd =
  CmdItemDetails
    <$> strOption
      (long "id" <> short 'i' <> help "ID of the item to show details for")

transferCommands :: Parser Command
transferCommands =
  subparser
    ( command
        "create"
        ( info
            (transferCreateCmd <**> helper)
            ( progDesc "Start downloading from a URL (magnet, torrent, or direct link)"
                <> footer "Examples:\n  transfer create --src 'magnet:?xt=urn:btih:...'\n  transfer create --src 'https://example.com/file.zip' --folder-id abc123"
            )
        )
        <> command
          "directdl"
          ( info
              (transferDirectDLCmd <**> helper)
              ( progDesc "Generate a direct download link without storing the file"
                  <> footer "Example:\n  transfer directdl --src 'https://example.com/file.zip'"
              )
          )
        <> command
          "list"
          ( info
              (pure CmdTransferList <**> helper)
              ( progDesc "Show all downloads with their status and progress"
                  <> footer "Shows: ID | Name | Status | Progress | Source URL"
              )
          )
        <> command
          "delete"
          ( info
              (transferDeleteCmd <**> helper)
              ( progDesc "Cancel and remove a download"
                  <> footer "Example:\n  transfer delete --id abc123"
              )
          )
    )

transferCreateCmd :: Parser Command
transferCreateCmd =
  CmdTransferCreate
    <$> strOption
      (long "src" <> short 's' <> help "Source URL (magnet link, torrent URL, or direct download link)")
    <*> optional
      (strOption (long "folder-id" <> short 'f' <> help "Destination folder ID (saves to root if not specified)"))

transferDirectDLCmd :: Parser Command
transferDirectDLCmd =
  CmdTransferDirectDL
    <$> strOption
      (long "src" <> short 's' <> help "Source URL to generate direct download link for")

transferDeleteCmd :: Parser Command
transferDeleteCmd =
  CmdTransferDelete
    <$> strOption
      (long "id" <> short 'i' <> help "ID of the transfer to cancel and delete")

cacheCommands :: Parser Command
cacheCommands =
  subparser
    ( command
        "check"
        ( info
            (cacheCheckCmd <**> helper)
            ( progDesc "Check if files are available in the Premiumize cache"
                <> footer "Example:\n  cache check --item 'filename.zip' --item 'otherhash'"
            )
        )
    )

cacheCheckCmd :: Parser Command
cacheCheckCmd =
  CmdCacheCheck
    <$> some
      (strOption (long "item" <> short 'i' <> help "Hash or filename to check (can be used multiple times)"))

servicesCommands :: Parser Command
servicesCommands =
  subparser
    ( command
        "list"
        ( info
            (pure CmdServicesList <**> helper)
            ( progDesc "Show all supported download services and file hosters"
                <> footer "Example:\n  services list"
            )
        )
    )

-- Main parsing function
parseOptions :: IO (GlobalOptions, Command)
parseOptions =
  customExecParser (prefs showHelpOnEmpty) $
    info
      ((,) <$> globalOptionsParser <*> commandParser <**> helper)
      ( fullDesc
          <> progDesc headerText
          <> OA.header versionText
          <> footer footerText
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
