;;; lang-init.el --- Language support configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Programming language support with performance considerations

;;; Code:

;; Tree-sitter configuration (for syntax highlighting)
(require 'treesit-config)

;; JavaScript configuration
(require 'js-config)

;; TypeScript configuration
(require 'ts-config)

;; TSX mode configuration
(require 'tsx-mode-config)

;; React/JSX configuration
(require 'jsx-config)
(require 'react-config)

;; CSS/Tailwind configuration
(require 'css-config)

;; Python
(defun my/python-mode-setup ()
  "Setup for python-mode."
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4))

(add-hook 'python-mode-hook 'my/python-mode-setup)

;; Go
(defun my/go-mode-setup ()
  "Setup for go-mode."
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save nil t))

(add-hook 'go-mode-hook 'my/go-mode-setup)

;; Rust
(defun my/rust-mode-setup ()
  "Setup for rust-mode."
  (setq rust-format-on-save t))

(add-hook 'rust-mode-hook 'my/rust-mode-setup)

;; Org mode configuration
(require 'org-config)

;; Disable syntax highlighting in large files for performance
(defun my/disable-syntax-highlighting-if-large ()
  "Disable syntax highlighting in large buffers."
  (when (> (buffer-size) (* 1000 1000)) ; 1MB
    (syntax-table (copy-syntax-table))
    (modify-syntax-entry ?_ "w")
    (jit-lock-mode -1)))

(add-hook 'find-file-hook 'my/disable-syntax-highlighting-if-large)

(provide 'lang-init)
;;; lang-init.el ends here