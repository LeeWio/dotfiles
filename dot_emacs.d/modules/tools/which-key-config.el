;;; which-key-config.el --- Enhanced Which Key configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Enhanced configuration for which-key package with custom settings

;;; Code:

;; Install and configure which-key
(use-package which-key
  :ensure t
  :defer 1
  :config
  ;; Enable which-key globally
  (which-key-mode 1)
  
  ;; Timing configuration
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.1)
  
  ;; Display configuration
  (setq which-key-max-description-length 40)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-slot -10)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-popup-type 'side-window)
  
  ;; Appearance customization
  (setq which-key-separator " → ")
  (setq which-key-prefix-prefix "+" )
  
  ;; Show which-key for C-h
  (which-key-setup-side-window-bottom)
  
  ;; Custom key binding descriptions
  (which-key-add-key-based-replacements
    "C-c !" "flycheck"
    "C-c @" "hideshow"
    "C-c b" "battery"
    "C-c c" "calendar"
    "C-c d" "dictionary"
    "C-c f" "file"
    "C-c g" "git"
    "C-c h" "help"
    "C-c i" "insert"
    "C-c j" "avy jump"
    "C-c k" "custom keys"
    "C-c l" "list"
    "C-c m" "mode"
    "C-c n" "next para"
    "C-c o" "open"
    "C-c p" "prev para"
    "C-c q" "quit"
    "C-c r" "register"
    "C-c s" "search"
    "C-c t" "toggle"
    "C-c u" "undo"
    "C-c v" "view"
    "C-c w" "window"
    "C-c x" "text"
    "C-c y" "yank"
    "C-c z" "zoom"
    "C-c SPC" "avy char"
    "C-c /" "comment"
    "C-x a" "abbrev"
    "C-x n" "narrow"
    "C-x r" "register"
    "C-x v" "vc"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x RET" "coding-system"
    "C-x 8" "unicode"
    "C-x =" "info"
    "C-x X" "edebug"
    "C-x {" "enlarge"
    "C-x }" "shrink"
    "C-x ^" "enlarge-window"
    "C-x 0" "delete-window"
    "C-x 1" "delete-other-windows"
    "C-x 2" "split-window-below"
    "C-x 3" "split-window-right"
    "C-x o" "other-window"
    "C-x b" "switch-buffer"
    "C-x C-b" "list-buffers"
    "C-x C-c" "save-buffers-kill-terminal"
    "C-x C-f" "find-file"
    "C-x C-s" "save-buffer"
    "C-x C-w" "write-file"
    "C-x C-z" "suspend-frame"
    "C-x k" "kill-buffer"
    "C-x u" "undo"
    "C-x z" "repeat")
  
  ;; Add custom keymaps
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c C-a" "attachments"
    "C-c C-b" "babel"
    "C-c C-c" "command"
    "C-c C-e" "export"
    "C-c C-f" "facilities"
    "C-c C-o" "open"
    "C-c C-q" "subtrees"
    "C-c C-r" "refile"
    "C-c C-s" "sparse"
    "C-c C-t" "tables"
    "C-c C-v" "views"
    "C-c C-w" "worg"
    "C-c C-x" "special")
  
  ;; Add more major mode replacements
  (which-key-add-major-mode-key-based-replacements 'python-mode
    "C-c C-a" "align"
    "C-c C-b" "breakpoint"
    "C-c C-c" "execute"
    "C-c C-d" "debug"
    "C-c C-e" "execute"
    "C-c C-f" "format"
    "C-c C-i" "inspect"
    "C-c C-j" "jump"
    "C-c C-k" "kill"
    "C-c C-l" "load"
    "C-c C-n" "navigate"
    "C-c C-p" "profile"
    "C-c C-r" "run"
    "C-c C-s" "send"
    "C-c C-t" "test"
    "C-c C-u" "unittest"
    "C-c C-v" "virtualenv"
    "C-c C-w" "whitespace"
    "C-c C-x" "execute")
  
  ;; Sort bindings alphabetically
  (setq which-key-sort-order 'which-key-key-order-alpha)
  
  ;; Don't show key sequences that are incomplete
  (setq which-key-show-prefix 'left)
  
  ;; Add padding around the popup
  (setq which-key-add-column-padding 1)
  
  ;; Highlight typed prefix
  (setq which-key-highlight-prefix t))

;; Add custom keymap for our prefix
(which-key-add-key-based-replacements
  "C-c k b" "buffers"
  "C-c k f" "files"
  "C-c k p" "projects"
  "C-c k t" "text"
  "C-c k l" "lines"
  "C-c k c" "compile"
  "C-c k d" "debug"
  "C-c k w" "windows"
  "C-c k u" "ui"
  "C-c k h" "help"
  "C-c k s" "search"
  "C-c s" "selection")

(provide 'which-key-config)
;;; which-key-config.el ends here