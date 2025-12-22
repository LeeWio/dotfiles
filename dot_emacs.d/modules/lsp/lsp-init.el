;;; lsp-init.el --- LSP initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Main initialization file for LSP and completion system using eglot

;;; Code:

;; Add language-specific configuration path
(add-to-list 'load-path (expand-file-name "modules/lsp/lang" user-emacs-directory))

;; Load completion system
(require 'corfu-config)
(message "Loaded corfu-config")

;; Load performance optimizations
(require 'lsp-perf)
(message "Loaded lsp-perf")

;; Load C/C++ specific eglot configuration
(require 'eglot-c)
(message "Loaded eglot-c")

(provide 'lsp-init)
;;; lsp-init.el ends here