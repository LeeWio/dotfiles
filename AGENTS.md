# AGENTS.md

This file provides guidance to Qoder (qoder.com) when working with code in this repository.

## Architecture Overview

This is a modular Emacs configuration optimized for fast startup, clean organization, and Java/TypeScript development.

### Directory Structure

```
~/.emacs.d/
├── init.el                 # Main entry point
├── early-init.el          # Pre-startup optimizations
├── core/                  # Core functionality
│   ├── init-benchmark.el  # Performance measurement
│   └── init-vars.el       # Global variables
├── modules/               # Feature modules
│   ├── init-ui.el         # UI configuration
│   ├── init-packages.el   # Package management
│   ├── init-editing.el    # Editing enhancements
│   ├── init-keybindings.el # Key bindings
│   ├── init-java.el       # Java-specific config
│   ├── init-typescript.el # TypeScript/JS config
│   └── init-lang.el       # General language support
├── extensions/            # Optional extensions
│   └── compile-config.el  # Config compilation
└── config/                # Local overrides (git-ignored)
    └── local-settings.el
```

### Loading Strategy

**Immediate loading** (during startup):
- init-ui.el - UI必须立即可见
- init-packages.el - 包管理和LSP基础
- init-java.el - Java LSP hooks
- init-typescript.el - TypeScript LSP hooks  
- init-lang.el - Web开发工具

**Deferred loading** (0.1s idle timer):
- init-editing.el
- init-keybindings.el
- compile-config.el

**Why this order matters**: LSP hooks MUST be registered BEFORE opening files, otherwise LSP won't auto-start.

## Development Commands

### Compile Configuration
```elisp
M-x my/compile-config
;; or
C-c c c
```
Byte-compiles all .el files for better performance.

### Install Icon Fonts
After installing all-the-icons:
```elisp
M-x all-the-icons-install-fonts
```

## Language Configuration

### Java (lsp-java)
- **Requirements**: JDK 17+ (currently using Java 24)
- **Language Server**: Eclipse JDT LS (auto-installed)
- **Hook**: `(add-hook 'java-mode-hook #'lsp)` in init-java.el
- **Project requirements**: pom.xml or build.gradle in project root
- **First startup**: May take minutes to download and index

**Configuration** (`~/.emacs.d/modules/init-java.el`):
```elisp
(setq lsp-java-vmargs '("-Xmx2G" "-Xms100m" ...))
(setq lsp-java-format-enabled t)
(setq lsp-java-save-actions-organize-imports t)
```

### TypeScript/JavaScript (typescript-mode)
- **Language Server**: typescript-language-server (install via npm)
- **Hook**: `(add-hook 'typescript-mode-hook #'lsp)` in init-typescript.el
- **File associations**: .ts, .tsx, .js, .jsx
- **Installation**:
  ```bash
  npm install -g typescript-language-server typescript
  npm install -g @tailwindcss/language-server
  npm install -g prettier
  ```

**Configuration** (`~/.emacs.d/modules/init-typescript.el`):
- typescript-mode: .ts/.tsx files
- js2-mode: .js files
- rjsx-mode: .jsx files

## Completion System

**Primary**: Company Mode (LSP official recommendation)
- Auto-triggers after 0.1s
- Minimum 1 character to trigger
- `TAB` or `RET` to complete
- `C-n`/`C-p` to navigate

**UI**: company-box for icons and documentation

**Key bindings** (when completion menu active):
- `TAB` / `RET` - Insert completion
- `C-n` - Next candidate
- `C-p` - Previous candidate

## LSP Features

### Key Bindings (prefix: C-c l)
- `C-c l g g` - Go to definition
- `C-c l g r` - Find references  
- `C-c l r r` - Rename symbol
- `C-c l a a` - Code actions
- `C-c l = =` - Format document

### Auto-start Requirements
LSP will auto-start when:
1. Language-specific hook is registered (in init-java.el or init-typescript.el)
2. File is in a valid project (has pom.xml/build.gradle/package.json)
3. Language server is installed

### Manual LSP Start
If LSP doesn't auto-start:
```
M-x lsp
```

## Theme and UI

- **Theme**: Catppuccin Mocha
- **Modeline**: doom-modeline with transparent background
- **Icons**: all-the-icons (requires font installation)
- **Git indicators**: diff-hl (shows changes in fringe)

## Performance Tuning

**GC Strategy**:
- Startup: 100MB threshold (early-init.el)
- Post-startup: 2MB threshold (init.el)

**Startup time**: Typically 0.2-0.3 seconds

**Benchmark**: `(my/elapsed-time)` shows time since startup

## Git Integration

- **magit**: Full Git interface
- **diff-hl**: Visual diff indicators
- **git-timemachine**: Browse file history (`C-x v t`)

## Common Issues

### LSP not auto-starting
1. Check hook is registered: `M-: java-mode-hook` (should show `(lsp)`)
2. Verify init-java.el was loaded: `M-x eval-expression RET (featurep 'init-java)`
3. If nil, language config wasn't loaded - check init.el loading order
4. Manual fix: `M-x lsp` then restart file

### No code completion
1. Verify LSP connected: Look for `LSP :: jdtls` or `LSP :: ts-ls` in modeline
2. Check company-mode active: `M-: company-mode` should return `t`
3. Trigger manually: Type and press `TAB`

### Java LSP slow
Increase heap: Edit `lsp-java-vmargs` in init-java.el

### TypeScript server missing
```bash
npm install -g typescript-language-server typescript
```

## Package Management

All packages managed via MELPA/GNU ELPA with use-package.

**Key packages**:
- LSP: lsp-mode, lsp-ui, lsp-java
- Completion: company, company-box
- Navigation: vertico, consult, marginalia
- Git: magit, diff-hl, git-timemachine
- Dev tools: flycheck, yasnippet, projectile

## Customization

Local customizations go in `~/.emacs.d/config/local-settings.el` (git-ignored).

Example:
```elisp
;; Override any settings
(setq company-idle-delay 0.2)
(setq lsp-java-vmargs '("-Xmx4G" ...))
```