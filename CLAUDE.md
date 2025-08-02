# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Haskell command-line interface for managing downloads, transfers, and cloud storage on Premiumize.me. The CLI provides a structured way to interact with the Premiumize.me API for various operations including account management, file operations, folder management, and transfer operations.

## Build System and Development Commands

This project uses:
- **Cabal** for Haskell package management and building
- **Nix Flakes** for reproducible development environment and builds

### Essential Commands

```bash
# Development environment (uses Nix flake)
nix develop                   # Enter development shell with all tools

# Building and running
cabal build                   # Build the project
cabal run                     # Run the CLI tool
cabal run -- <args>           # Run with arguments
nix run                       # Run via Nix (after build)

# Testing
cabal test                    # Run test suite (currently minimal)

# Development tools
cabal repl                    # Start REPL for interactive development
hlint src/                    # Lint Haskell code
ormolu --mode inplace src/    # Format Haskell code
```

### Nix-specific Commands

```bash
nix build                     # Build the package
nix run                       # Run the built executable
nix flake check              # Run all checks including format-check
nixpkgs-fmt flake.nix        # Format Nix files
```

## Architecture

The codebase follows a clean three-layer architecture:

### Core Modules

1. **`PremiumizeCli.Types`** (`src/PremiumizeCli/Types.hs`)
   - Defines all data types for API responses and CLI commands
   - Uses Aeson for JSON serialization/deserialization
   - Contains `GlobalOptions` for CLI-wide settings (JSON output, verbose mode)
   - Comprehensive `Command` ADT covering all CLI operations

2. **`PremiumizeCli.API`** (`src/PremiumizeCli/API.hs`)
   - Pure API client implementation using `wreq` HTTP library
   - Handles authentication via `PREMIUMIZE_API_KEY` environment variable
   - Implements all Premiumize.me API endpoints
   - Error handling with proper JSON/text output modes
   - Base URL: `https://www.premiumize.me/api`

3. **`PremiumizeCli.CLI`** (`src/PremiumizeCli/CLI.hs`)
   - Command-line interface using `optparse-applicative`
   - Hierarchical command structure (account, transfer, folder, item, cache, services)
   - Comprehensive help text and examples
   - Maps parsed commands to API functions

### Key API Operations

The CLI supports these main operation categories:
- **Account**: Information and usage statistics
- **Transfers**: Create, list, delete downloads; direct download links
- **Folders**: List, create, rename, delete, search folder structure
- **Items**: List all, delete, rename, get details for individual files
- **Cache**: Check file availability in Premiumize cache
- **Services**: List supported download services

### Environment Configuration

- **Required**: `PREMIUMIZE_API_KEY` environment variable
- **Optional**: `.envrc` file with direnv for local development (uses `pass` for API key storage)

## Development Notes

- The project uses modern Haskell with language extensions: `OverloadedStrings`, `DeriveGeneric`, `RecordWildCards`, `ScopedTypeVariables`
- Follows functional programming principles with pure functions and controlled effects
- Error handling uses Either/Maybe patterns and exception handling for HTTP operations
- JSON output mode available for all commands via `--json` flag
- Comprehensive command-line help with examples for each operation
- Currently minimal test suite - tests need to be implemented in `test/Spec.hs`

## Code Quality

The project is configured with strict GHC warnings in `cabal-file`:
- `-Wall`, `-Wcompat`, `-Widentities`, `-Wincomplete-record-updates`, `-Wincomplete-uni-patterns`, `-Wpartial-fields`, `-Wredundant-constraints`
- Use `hlint` for additional linting
- Use `ormolu` for consistent code formatting