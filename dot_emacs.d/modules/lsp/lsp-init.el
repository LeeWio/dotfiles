;;; lsp-init.el --- LSP initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Main initialization file for LSP and completion system using eglot

;;; Code:

;; Add language-specific configuration path
(add-to-list 'load-path (expand-file-name "modules/lsp/lang" user-emacs-directory))

;; Completion & Eglot system
(require 'corfu-config)
(message "Loaded corfu-config")

;; Performance tuning (你的文件保留即可)
(require 'lsp-perf)
(message "Loaded lsp-perf")

;; C/C++ specific eglot config
(require 'eglot-c)
(message "Loaded eglot-c")

(require 'eglot-java)  ;; Java
(message "Loaded eglot-java")

(require 'eglot-ts-config)   ;; TypeScript / JavaScript
(message "Loaded eglot-ts-config")

(provide 'lsp-init)
;;; lsp-init.el ends here