;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Early initialization to maximize performance

;;; Code:

;; Disable GUI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Resize frame early
(push '(width . 120) default-frame-alist)
(push '(height . 40) default-frame-alist)

;; Disable package initialization
(setq package-enable-at-startup nil)

;; Prefer loading .el files over .elc files to avoid bytecode issues
(setq load-prefer-newer t)

;; Garbage collection during startup
(setq gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-percentage 0.1)

;; Frame resizing
(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here