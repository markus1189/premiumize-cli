# premiumize-cli

A modern Haskell command-line interface for managing downloads, transfers, and cloud storage on [Premiumize.me](https://premiumize.me/).

## Features

- **Account Management**: View account information and usage statistics
- **Transfer Operations**: Create, list, and manage downloads
- **Folder Management**: Navigate, create, rename, and delete folders
- **File Operations**: List, rename, delete, and get details for files
- **Cache Operations**: Check file availability in Premiumize cache
- **Service Support**: List all supported download services
- **JSON Output**: Machine-readable output for scripting and automation
- **Comprehensive Help**: Detailed help text and examples for all commands

## Installation

### Prerequisites

- [Nix](https://nixos.org/download.html) (recommended for reproducible builds)
- Or [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) for traditional Haskell development

### Using Nix (Recommended)

```bash
# Clone the repository
git clone https://github.com/markus1189/premiumize-cli.git
cd premiumize-cli

# Build and run
nix run
```

### Using Cabal

```bash
# Clone the repository
git clone https://github.com/markus1189/premiumize-cli.git
cd premiumize-cli

# Build
cabal build

# Run
cabal run
```

## Configuration

Set your Premiumize.me API key as an environment variable:

```bash
export PREMIUMIZE_API_KEY="your-api-key-here"
```

You can find your API key in your [Premiumize.me account settings](https://www.premiumize.me/account).

For development with [direnv](https://direnv.net/), create a `.envrc` file:

```bash
export PREMIUMIZE_API_KEY=$(pass premiumize/api-key)  # or however you store secrets
```

## Usage

### Basic Commands

```bash
# View account information
premiumize-cli account info

# List transfers
premiumize-cli transfer list

# Create a new download
premiumize-cli transfer create "magnet:?xt=urn:btih:..."

# List folder contents
premiumize-cli folder list

# Get direct download link
premiumize-cli transfer directdl <transfer-id>
```

### Command Structure

The CLI follows a hierarchical command structure:

- `account` - Account operations
  - `info` - Show account information
- `transfer` - Transfer operations
  - `list` - List all transfers
  - `create <src>` - Create new transfer
  - `delete <id>` - Delete transfer
  - `directdl <id>` - Get direct download link
- `folder` - Folder operations
  - `list [id]` - List folder contents
  - `create <name> [parent-id]` - Create folder
  - `rename <id> <name>` - Rename folder
  - `delete <id>` - Delete folder
  - `search <query>` - Search folders
- `item` - File operations
  - `list` - List all items
  - `delete <id>` - Delete item
  - `details <id>` - Get item details
  - `rename <id> <name>` - Rename item
- `cache` - Cache operations
  - `check <items...>` - Check cache status
- `services` - Service operations
  - `list` - List supported services

### Global Options

- `--json` - Output results in JSON format for scripting
- `--verbose` - Enable verbose output for debugging

### Examples

```bash
# Check account status with JSON output
premiumize-cli --json account info

# Create a transfer and get its ID
TRANSFER_ID=$(premiumize-cli --json transfer create "magnet:..." | jq -r '.id')

# Search for files containing "movie"
premiumize-cli folder search "movie"

# Check if files are available in cache
premiumize-cli cache check "http://example.com/file1.zip" "http://example.com/file2.rar"

# Get direct download link for a transfer
premiumize-cli transfer directdl $TRANSFER_ID
```

## Development

### Using Nix (Recommended)

```bash
# Enter development environment
nix develop

# Build the project
cabal build

# Run tests
cabal test

# Start REPL
cabal repl

# Format code
ormolu --mode inplace src/

# Lint code
hlint src/

# Run all checks
nix flake check
```

### Project Structure

```
src/
├── PremiumizeCli/
│   ├── Types.hs      # Data types and JSON instances
│   ├── API.hs        # HTTP API client
│   └── CLI.hs        # Command-line interface
└── Main.hs           # Entry point
```

### Architecture

The project follows a clean three-layer architecture:

1. **Types Layer** (`PremiumizeCli.Types`) - All data types and JSON serialization
2. **API Layer** (`PremiumizeCli.API`) - Pure HTTP client implementation
3. **CLI Layer** (`PremiumizeCli.CLI`) - Command-line interface and option parsing

## API Coverage

This CLI implements the complete Premiumize.me API:

- ✅ Account information and usage
- ✅ Transfer management (create, list, delete, direct download)
- ✅ Folder operations (list, create, rename, delete, search)
- ✅ Item management (list, delete, rename, details)
- ✅ Cache checking
- ✅ Service listing

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes following the existing code style
4. Run the linter and formatter: `hlint src/` and `ormolu --mode inplace src/`
5. Commit your changes (`git commit -am 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Premiumize.me](https://premiumize.me/) for providing the API
- The Haskell community for excellent libraries like `wreq`, `aeson`, and `optparse-applicative`