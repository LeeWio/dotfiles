;;; tools-init.el --- Tools configurations -*- lexical-binding: t -*-

;;; Commentary:
;; External tools and utilities configuration

;;; Code:

;; Dired
(setq dired-listing-switches "-alh --group-directories-first")
(setq dired-dwim-target t)

;; TRAMP
(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; Magit performance
(setq magit-refresh-status-buffer nil)
(setq magit-auto-revert-mode-lighter "")

;; Org-mode performance
(setq org-element-cache-persistent nil)
(setq org-element-use-cache nil)
(setq org-agenda-files '())

;; Comint (shell) performance
(setq comint-buffer-maximum-size 2048)
(setq comint-prompt-read-only t)

;; Undo-tree
(setq undo-limit 800000)
(setq undo-strong-limit 96000000)
(setq undo-outer-limit 12000000)

;; Project
(setq project-list-file (expand-file-name "var/projects" user-emacs-directory))

;; Which-key configuration
(require 'which-key-config)

;; Git configuration
(require 'git-config)

;; MWIM configuration
(require 'mwim-config)

;; Clipboard configuration for Wayland
(require 'clipboard-config)

;; Frontend formatting tools
(require 'frontend-formatting)

;; Frontend project management
(require 'frontend-project)

;; Projectile configuration
(require 'projectile-init)

;; Treemacs configuration
(require 'treemacs-init)

;; Vertico configuration
(require 'vertico-init)

(provide 'tools-init)
;;; tools-init.el ends here