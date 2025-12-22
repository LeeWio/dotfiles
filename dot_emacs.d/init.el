;;; init.el --- My Emacs Configuration -*- lexical-binding: t -*-

;; Author: Wei Li
;; Keywords: convenience

;;; Commentary:
;; High-performance modular Emacs configuration

;;; Code:

;; Package management
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Performance optimizations
(setq gc-cons-threshold (* 100 1024 1024)) ; 100MB
(setq read-process-output-max (* 1024 1024)) ; 1MB
(setq native-comp-async-report-warnings-errors 'silent)

;; Load path
(add-to-list 'load-path (expand-file-name "modules/core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/ui" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/tools" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/perf" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/lsp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/lsp/lang" user-emacs-directory))

;; Core configurations
(require 'core-init)

;; UI configurations
(require 'ui-init)

;; Language support
(require 'lang-init)

;; Tools configurations
(require 'tools-init)

;; Performance tuning
(require 'perf-init)

;; LSP and completion system
(require 'lsp-init)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )