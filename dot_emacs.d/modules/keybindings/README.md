# Emacs Keybindings Configuration

This configuration provides a comprehensive set of keybindings to enhance your Emacs productivity. All custom keybindings are organized under the `C-c k` prefix for easy discovery.

## Custom Keybindings Prefix

**Prefix:** `C-c k` - All custom keybindings start with this prefix

## Navigation & Buffer Management (`C-c k b`)

- `C-c k b n` - Next buffer (`next-buffer`)
- `C-c k b p` - Previous buffer (`previous-buffer`)
- `C-c k b k` - Kill current buffer (`kill-this-buffer`)
- `C-c k b s` - Switch to buffer (`switch-to-buffer`)
- `C-c k b l` - List buffers (`ibuffer`)

## File Operations (`C-c k f`)

- `C-c k f f` - Find file (`find-file`)
- `C-c k f s` - Save buffer (`save-buffer`)
- `C-c k f w` - Write file (`write-file`)
- `C-c k f r` - Open recent files (`recentf-open-files`)

## Project Management (`C-c k p`)

- `C-c k p f` - Find file in project (requires projectile)
- `C-c k p p` - Switch project (requires projectile)
- `C-c k p s` - Search in project (requires projectile)

## Text Manipulation (`C-c k t`)

- `C-c k t c` - Capitalize word/region (`capitalize-dwim`)
- `C-c k t l` - Downcase word/region (`downcase-dwim`)
- `C-c k t u` - Upcase word/region (`upcase-dwim`)
- `C-c k t r` - Reverse region (`reverse-region`)
- `C-c k t f` - Fill paragraph (`fill-paragraph`)

## Line Operations (`C-c k l`)

- `C-c k l d` - Duplicate line/region (`duplicate-dwim`)
- `C-c k l c` - Delete trailing whitespace (`delete-trailing-whitespace`)
- `C-c k l n` - Move to column (`move-to-column`)
- `C-c k l s` - Sort lines (`sort-lines`)

## Compilation & Building (`C-c k c`)

- `C-c k c c` - Compile project (`compile`)
- `C-c k c r` - Recompile (`recompile`)

## Debugging (`C-c k d`)

- `C-c k d d` - Start GDB (`gdb`)
- `C-c k d t` - Toggle debug on error (`toggle-debug-on-error`)

## Window Management (`C-c k w`)

- `C-c k w s` - Split window horizontally (`split-window-below`)
- `C-c k w v` - Split window vertically (`split-window-right`)
- `C-c k w c` - Close current window (`delete-window`)
- `C-c k w o` - Close other windows (`delete-other-windows`)

## UI Toggles (`C-c k u`)

- `C-c k u l` - Toggle line numbers (`global-display-line-numbers-mode`)
- `C-c k u h` - Toggle highlight current line (`global-hl-line-mode`)
- `C-c k u w` - Toggle whitespace visualization (`whitespace-mode`)
- `C-c k u f` - Toggle auto-fill mode (`auto-fill-mode`)

## Help & Documentation (`C-c k h`)

- `C-c k h k` - Describe key (`describe-key`)
- `C-c k h f` - Describe function (`describe-function`)
- `C-c k h v` - Describe variable (`describe-variable`)
- `C-c k h m` - Describe mode (`describe-mode`)

## Search & Replace (`C-c k s`)

- `C-c k s s` - Incremental search forward (`isearch-forward`)
- `C-c k s r` - Query replace (`query-replace`)
- `C-c k s f` - Find grep (`find-grep`)

## Text Selection

### Progressive Selection (`C-=` and `C--`)
Using expand-region for smart selection:
- `C-=` - Expand selection outward (character → word → line → paragraph → function)
- `C--` - Contract selection inward

### Visual Selection with Avy
- `C-c SPC` - Avy character selection (type characters to jump anywhere)
- `C-c j` - Avy line selection (type line number to jump)

### Rectangle Selection
- `C-x SPC` - Start rectangle selection mode

### Smart Movement with Selection
- `S-<right>` - Select next character
- `S-<left>` - Select previous character
- `S-C-<right>` - Select next word
- `S-C-<left>` - Select previous word
- `S-C-<up>` - Select previous line
- `S-C-<down>` - Select next line

### Quick Selection Helpers
- `C-c s l` - Select current line
- `C-c s p` - Select current paragraph
- `C-c s f` - Select current function

## Global Keybindings

### Window Navigation
- `C-x <up>` - Move to window above (`windmove-up`)
- `C-x <down>` - Move to window below (`windmove-down`)
- `C-x <left>` - Move to window left (`windmove-left`)
- `C-x <right>` - Move to window right (`windmove-right`)

### Quick Access
- `C-x C-b` - IBuffer (`ibuffer`)
- `C-c SPC` - Avy goto char timer (`avy-goto-char-timer`)
- `C-c j` - Avy goto line (`avy-goto-line`)
- `C-c /` - Comment/uncomment region (`comment-or-uncomment-region`)
- `C-c n` - Forward paragraph (`forward-paragraph`)
- `C-c p` - Backward paragraph (`backward-paragraph`)

### Enhanced Navigation
- `C-a` - Move to beginning of line (smart) (`mwim-beginning-of-line`)
- `C-e` - Move to end of line (smart) (`mwim-end-of-line`)

### Quick Save
- `C-x C-s` - Save buffer (`save-buffer`)

## Discoverability

All keybindings are integrated with `which-key`, which shows available keybindings when you pause after pressing a prefix key. For example, after pressing `C-c k`, a popup will show all available options under this prefix.