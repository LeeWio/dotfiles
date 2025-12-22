# LSP and Completion System Setup

This configuration uses `eglot` for language server protocol support and `corfu` for completion system, focused specifically on C/C++ development.

## Required Packages

Install the following packages using your system package manager or via Emacs package manager:

1. **Corfu** - Modern completion UI
2. **Orderless** - Flexible completion matching
3. **Cape** - Completion At Point Extensions
4. **Eldoc-box** - Enhanced documentation display

Note: Eglot is built into Emacs 29+, so no additional installation is required.

## Language Server Installation

### C/C++
```bash
# Ubuntu/Debian
sudo apt install clangd

# macOS
brew install llvm
```

## Key Bindings

### Eglot Commands
- `C-c r` - Rename symbol
- `C-c g` - Go to definition
- `C-c b` - Go to declaration
- `C-c p` - Find references
- `C-c h` - Show documentation (hover)
- `C-c i` - Find implementation
- `C-c f` - Format code
- `C-c a` - Code actions

### Completion Commands
- `TAB` - Next completion
- `S-TAB` - Previous completion
- `M-d` - Toggle documentation popup
- `M-l` - Show location

## Completion System Features

The completion system combines several technologies for an optimal experience:

1. **Corfu** - Provides a clean, modern completion UI
2. **Orderless** - Enables flexible, multi-pattern matching
3. **Cape** - Adds various completion sources (dictionaries, file paths, etc.)
4. **Eglot** - Provides LSP-based intelligent code completion
5. **Eldoc-box** - Shows documentation in an overlay

## Performance Features

- Automatic throttling for large files
- Configurable completion delays
- Efficient memory management
- Lazy loading of language servers

## Modular Structure

The configuration is organized in a modular way:

- `corfu-config.el` - Completion system configuration
- `lsp-perf.el` - Performance optimizations
- `lang/eglot-c.el` - C/C++ specific eglot configuration

This modular structure makes it easy to troubleshoot and extend with additional languages.