# LeeWio's Dotfiles

A complete, modern development environment configuration managed with [chezmoi](https://www.chezmoi.io/).

## Overview

This repository contains my personal dotfiles for a productive development environment featuring:

- **Emacs** - Highly customized configuration with LSP support for multiple languages
- **Kitty** - GPU-accelerated terminal emulator with Catppuccin theme
- **Modular Structure** - Easy to understand and customize configuration files

## Features

### Emacs Configuration
- **Theme**: Catppuccin Mocha - Beautiful, consistent dark theme
- **Languages**: Full LSP support for:
  - C/C++ (with clangd)
  - JavaScript/TypeScript (with typescript-language-server)
  - Go (with gopls)
  - Python (basic support)
- **UI Enhancements**:
  - Clean interface with hidden menu/toolbar/scrollbars
  - Transparent backgrounds
  - DOOM Modeline with VS Code-like appearance
  - Relative line numbers with current line highlighting
  - Smooth scrolling
- **Development Tools**:
  - Advanced auto-completion (Corfu, Vertico, Orderless)
  - Git integration (Magit, Git Gutter, Blamer)
  - Code formatting with clang-format
  - Performance optimized startup

### Kitty Terminal
- GPU-accelerated terminal emulator
- Catppuccin Mocha theme
- Optimized for development workflows

## Installation

### Prerequisites

Before installing, ensure you have the following installed:

```bash
# Install chezmoi
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply LeeWio

# Or using brew (macOS)
brew install chezmoi

# Install required development tools
# For macOS (using Homebrew):
brew install emacs kitty node npm clang-format

# For Ubuntu/Debian:
sudo apt install emacs kitty nodejs npm clang-format

# Install language servers
npm install -g typescript-language-server
```

### Apply Configuration

```bash
# Clone and apply dotfiles
chezmoi init https://github.com/LeeWio/dotfiles.git
chezmoi apply

# Or in one command:
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply LeeWio
```

## Manual Installation (Alternative)

If you prefer not to use chezmoi:

```bash
# Clone repository
git clone https://github.com/LeeWio/dotfiles.git ~/.dotfiles

# Link Emacs configuration
ln -sf ~/.dotfiles/dot_emacs.d ~/.emacs.d

# Link Kitty configuration
mkdir -p ~/.config/kitty
ln -sf ~/.dotfiles/dot_config/kitty/* ~/.config/kitty/
```

## Language Server Setup

For full LSP functionality, install the required language servers:

### JavaScript/TypeScript
```bash
npm install -g typescript-language-server typescript
```

### C/C++
```bash
# macOS
brew install llvm

# Ubuntu/Debian
sudo apt install clangd clang-format
```

### Go
```bash
# Install Go first, then:
go install golang.org/x/tools/gopls@latest
```

## Customization

### Emacs
- Configuration files are located in `~/.emacs.d/config/`
- Language-specific configurations in `~/.emacs.d/config/languages/`
- Add personal customizations in `~/.emacs.d/config/init-local.el`

### Kitty
- Configuration files in `~/.config/kitty/`
- Theme files in `~/.config/kitty/Catppuccin-Mocha.conf`

## Keybindings

### Emacs
- `C-c l r` - Rename symbol (LSP)
- `C-c l f` - Format buffer (LSP)
- `C-c l d` - Show documentation (LSP)
- `C-c g s` - Open Magit status
- `s-i` - Show Git blame info for current line
- `C-c i` - Show Git blame in posframe

### Kitty
- `cmd+enter` - New window
- `cmd+d` - New tab
- `cmd+shift+d` - Close tab
- `cmd+[` / `cmd+]` - Switch tabs

## Troubleshooting

### Theme Issues
If Catppuccin theme doesn't load:
1. Ensure `catppuccin-theme` package is installed
2. Restart Emacs
3. Run `M-x load-theme RET catppuccin RET`

### LSP Problems
If LSP servers aren't working:
1. Verify language servers are installed globally
2. Check `M-x lsp-describe-session` for diagnostics
3. Restart Emacs and reopen files

### Performance
If Emacs feels slow:
1. Check `M-x benchmark-init/show-durations-tabulate` for slow packages
2. Consider disabling unused language modules in `~/.emacs.d/config/init-languages.el`

## Updates

To update to the latest configuration:

```bash
chezmoi update
```

## Contributing

Feel free to fork this repository and adapt it to your needs. If you have suggestions for improvements, please open an issue or submit a pull request.

## License

These dotfiles are released under the MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

- [Catppuccin](https://github.com/catppuccin/catppuccin) - For the beautiful theme
- [DOOM Emacs](https://github.com/doomemacs/doomemacs) - Inspiration for modular configuration
- [chezmoi](https://github.com/twpayne/chezmoi) - Excellent dotfile manager