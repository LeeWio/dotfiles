;;; ui-init.el --- UI configurations -*- lexical-binding: t -*-

;;; Commentary:
;; UI customizations for better performance and appearance

;;; Code:

;; Catppuccin theme configuration
(require 'catppuccin-config)

;; Transparent background configuration
(require 'transparent-bg)

;; Mode line configuration
(require 'mode-line-config)

;; Fonts (only in GUI mode)
(when (display-graphic-p)
  (cond
   ((member "JetBrains Mono" (font-family-list))
    (set-face-attribute 'default nil :font "JetBrains Mono-14"))
   ((member "Maple Mono NF CN" (font-family-list))
    (set-face-attribute 'default nil :font "Maple Mono NF CN-14"))
   ((member ".SF NS Mono" (font-family-list))
    (set-face-attribute 'default nil :font ".SF NS Mono-14"))
   (t
    (set-face-attribute 'default nil :font "Monaco-14"))))

;; Modeline (skip in batch mode)
(unless noninteractive
  (column-number-mode 1)
  (size-indication-mode 1))

;; Disable fringe bitmaps for performance
(setq overflow-newline-into-fringe nil)

;; Reduce rendering updates
(setq redisplay-dont-pause t)
(setq fast-but-imprecise-scrolling t)

;; Disable window divider to prevent extra lines
(window-divider-mode -1)

;; Minibuffer
(setq resize-mini-windows t)
(setq max-mini-window-height 0.3)

(provide 'ui-init)
;;; ui-init.el ends here