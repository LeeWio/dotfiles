;;; core-init.el --- Core configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Core Emacs configurations

;;; Code:

;; Disable package.el in favor of straight.el or other package managers
(setq package-enable-at-startup nil)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Basic UI elements (only in GUI mode)
(when (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; Line number configuration
(require 'line-number-config)

;; Current line highlighting configuration
(require 'hl-line-config)

;; Smooth scrolling configuration
(require 'smooth-scrolling-config)

;; Whitespace
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; Backup and auto-save files
(setq backup-directory-alist `(("." . ,(expand-file-name "backup/" user-emacs-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
(setq create-lockfiles nil)

;; History
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq recentf-exclude '("/elpa/" "/var/" "/cache/"))

;; Save cursor position
(save-place-mode 1)

;; Better scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Enable some useful built-ins
(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

;; File type associations
(require 'file-associations)

;; Disable bidirectional text rendering for performance
(setq bidi-display-reordering 'left-to-right)
(setq bidi-paragraph-direction 'left-to-right)

(provide 'core-init)
;;; core-init.el ends here