;;; treemacs-config.el --- Treemacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Treemacs file explorer configuration

;;; Code:

;; Install treemacs if not already installed
(unless (package-installed-p 'treemacs)
  (package-refresh-contents)
  (package-install 'treemacs))

(unless (package-installed-p 'treemacs-icons-dired)
  (package-refresh-contents)
  (package-install 'treemacs-icons-dired))

;; Load treemacs
(require 'treemacs)
(require 'treemacs-icons-dired)

;; Set up key bindings for treemacs
(with-eval-after-load 'treemacs
  (define-key global-map (kbd "C-x t 1") #'treemacs)
  (define-key global-map (kbd "C-x t 8") #'treemacs-select-window)
  (define-key global-map (kbd "C-x t 3") #'treemacs-toggle)
  (define-key global-map (kbd "C-c t t") #'treemacs))

;; Configure treemacs settings
(setq treemacs-collapse-dirs 3)                       ; Collapse directories with single child
(setq treemacs-deferred-git-apply-delay 0.5)          ; Delay for git status updates
(setq treemacs-directory-name-transformer #'identity) ; No transformation of directory names
(setq treemacs-display-in-side-window t)              ; Display in side window
(setq treemacs-eldoc-display t)                       ; Enable eldoc display
(setq treemacs-file-event-delay 5000)                 ; Delay for file event processing
(setq treemacs-file-follow-delay 0.1)                 ; Delay for file following
(setq treemacs-follow-after-init t)                   ; Follow current file after init
(setq treemacs-git-command-pipe "")                   ; No additional git command pipe
(setq treemacs-goto-tag-strategy 'refetch-index)      ; Strategy for going to tags
(setq treemacs-indentation 2)                         ; Indentation level
(setq treemacs-indentation-string "  ")               ; Indentation string
(setq treemacs-is-never-other-window t)               ; Prevent treemacs from being other window
(setq treemacs-max-git-entries 5000)                  ; Max git entries to process
(setq treemacs-missing-project-action 'ask)           ; What to do when project is missing
(setq treemacs-move-forward-on-expand t)              ; Move forward when expanding
(setq treemacs-no-png-images nil)                     ; Allow PNG images
(setq treemacs-no-delete-other-windows t)             ; Don't delete other windows
(setq treemacs-project-follow-cleanup t)              ; Cleanup after project follow
(setq treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)) ; Persist file location
(setq treemacs-position 'left)                        ; Position of treemacs window
(setq treemacs-read-string-input 'from-child-frame)   ; Read string input method
(setq treemacs-recenter-after-file-follow nil)        ; Don't recenter after file follow
(setq treemacs-recenter-distance 0.1)                 ; Recenter distance
(setq treemacs-show-cursor nil)                       ; Don't show cursor in treemacs
(setq treemacs-show-hidden-files t)                   ; Show hidden files
(setq treemacs-silent-filewatch nil)                  ; Don't be silent about filewatch
(setq treemacs-silent-refresh nil)                    ; Don't be silent about refresh
(setq treemacs-sorting 'alphabetic-asc)               ; Sorting method
(setq treemacs-space-between-root-nodes nil)          ; No space between root nodes
(setq treemacs-tag-follow-cleanup t)                  ; Cleanup after tag follow
(setq treemacs-tag-follow-delay 1.5)                  ; Tag follow delay
(setq treemacs-width 35)                              ; Width of treemacs window

;; Enable icons in dired
(treemacs-icons-dired-mode)

;; Configure treemacs major mode
(with-eval-after-load 'treemacs
  ;; Define keybindings for treemacs-mode-map
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (define-key treemacs-mode-map [mouse-2] #'treemacs-double-click-expand-action))

(provide 'treemacs-config)
;;; treemacs-config.el ends here