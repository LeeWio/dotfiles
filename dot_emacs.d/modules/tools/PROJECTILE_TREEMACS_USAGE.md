# Projectile and Treemacs Usage Guide

## Projectile - Project Management

Projectile is a project interaction library for Emacs that provides easy navigation and management of projects.

### Key Features:
- Project discovery and indexing
- Fast file switching within projects
- Multi-project search and replace
- Integration with version control systems

### Essential Keybindings:
- `C-c p p` - Switch to a project
- `C-c p f` - Find file in project
- `C-c p g` - Find file with grep in project
- `C-c p s s` - Search in project with grep
- `C-c p s r` - Replace in project with regexp
- `C-c p d` - Run dired in project root
- `C-c p e` - Run eshell in project root
- `C-c p !` - Run shell command in project root
- `C-c p &` - Run async shell command in project root
- `C-c p b` - Switch to project buffer
- `C-c p o` - Run multi-occur in project buffers
- `C-c p k` - Kill all project buffers
- `C-c p r` - Replace in project files with find and replace
- `C-c p i` - Invalidate project cache
- `C-c p v` - Run vc-dir in project root
- `C-c p t` - Run test in project
- `C-c p T` - Run test in other window
- `C-c p a` - Run test-all in project
- `C-c p .` - Set current directory as a project

### Commands:
- `M-x projectile-switch-project` - Switch to a different project
- `M-x projectile-find-file` - Find file in project
- `M-x projectile-grep` - Grep in project
- `M-x projectile-replace` - Replace in project
- `M-x projectile-compile-project` - Compile project
- `M-x my/projectile-set-current-dir-as-project` - Set current directory as a project

## Treemacs - File Explorer

Treemacs is a tree layout file explorer for Emacs that provides a visual representation of your project structure.

### Key Features:
- Hierarchical file and directory browsing
- Project-aware file management
- Git status integration
- Multiple project support
- Customizable icons and appearance

### Essential Keybindings:
- `C-x t 1` - Show only treemacs
- `C-x t 8` - Show treemacs on the left
- `C-x t 3` - Toggle treemacs window
- `C-c t t` - Open treemacs
- `SPC` - Expand/collapse directory
- `TAB` - Expand/collapse directory (alternative)
- `RET` - Visit file/directory
- `C-c C-t o` - Toggle treemacs window
- `C-c C-t C-f` - Toggle treemacs follows file
- `C-c C-t f` - Toggle treemacs follows file
- `a` - Add a project to the workspace
- `r` - Remove project from workspace
- `R` - Rename path
- `D` - Delete file/directory
- `C` - Copy file/directory
- `c` - Create file/directory
- `?` - Show help

### Commands:
- `M-x treemacs` - Open treemacs
- `M-x treemacs-select-window` - Select treemacs window
- `M-x treemacs-toggle` - Toggle treemacs window
- `M-x treemacs-current-window` - Show treemacs in current window
- `M-x treemacs-find-file` - Show current file in treemacs
- `M-x treemacs-resize` - Resize treemacs window

## Integration between Projectile and Treemacs

The two tools work well together:

1. Projectile can be used to switch between projects
2. Treemacs can be used to navigate within a project
3. Use `C-c p p` to switch projects, then use treemacs for file navigation

## Tips and Tricks

### For Projectile:
- Projectile automatically detects projects based on version control directories (.git, .hg, etc.) or project files
- Use `C-c p i` to invalidate cache if files don't appear in search
- Projectile remembers recently opened projects for quick switching

### For Treemacs:
- Right-click (mouse-3) on nodes for context menu
- Use `.` to toggle hidden files visibility
- Use `q` to quit treemacs window
- Use `g` to refresh the view

## Troubleshooting

### If Projectile doesn't recognize a project:
- Make sure your project has a version control directory like .git
- Or create a `.projectile` file in the project root
- Run `C-c p i` to invalidate and rebuild the cache

### If Treemacs doesn't update file changes:
- Press `g` to manually refresh the view
- Check that filewatch is working properly

## Customization

The configuration files are located at:
- `/modules/tools/projectile-config.el` - Projectile settings
- `/modules/tools/treemacs-config.el` - Treemacs settings

You can modify these files to adjust settings like:
- Window width and position
- Sorting order
- Ignored directories/files
- Keybindings