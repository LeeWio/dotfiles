;;; lsp-init.el --- LSP initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Main initialization file for LSP and completion system using eglot

;;; Code:

;; Add language-specific configuration path
(add-to-list 'load-path (expand-file-name "modules/lsp/lang" user-emacs-directory))

;; Load completion system (includes eglot configuration)
(require 'corfu-config)
(message "Loaded corfu-config")

;; Load performance optimizations
(require 'lsp-perf)
(message "Loaded lsp-perf")

(provide 'lsp-init)
;;; lsp-init.el ends here