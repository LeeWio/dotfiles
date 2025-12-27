;;; tsx-config.el --- TSX specific configuration -*- lexical-binding: t -*-

;;; Commentary:
;; TSX-specific configuration for React/JSX in TypeScript files

;;; Code:

;; Special configuration for TSX files to ensure JSX support
(defun my/tsx-file-setup ()
  "Setup for TSX files."
  ;; This runs when a .tsx file is opened
  (when (string-match "\\.tsx\\'" (buffer-file-name))
    ;; Ensure we're in the right mode for JSX
    (setq-local lsp-idle-delay 0.5)
    ;; Enable JSX-specific features if available
    (setq-local typescript-indent-level 2)))

;; Run setup when opening TSX files in tsx-ts-mode
(add-hook 'tsx-ts-mode-hook 'my/tsx-file-setup)

(provide 'tsx-config)
;;; tsx-config.el ends here