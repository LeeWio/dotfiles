;;; hl-line-config.el --- Current line highlighting configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Simple and effective configuration for highlighting the current line

;;; Code:

;; Enable global hl-line mode
(global-hl-line-mode 1)

;; Simple hl-line face customization for Catppuccin theme
(set-face-background 'hl-line "#313244") ; Catppuccin surface0
(set-face-attribute 'hl-line nil :extend t)

;; Ensure hl-line works properly with transparency
(defun my/setup-hl-line-for-transparent-bg ()
  "Setup hl-line for transparent backgrounds."
  (when (facep 'hl-line)
    ;; Use a solid color that works well with transparent backgrounds
    (set-face-background 'hl-line "#313244")
    (set-face-attribute 'hl-line nil :extend t)))

;; Apply hl-line customization after theme is loaded
(add-hook 'after-init-hook 'my/setup-hl-line-for-transparent-bg)
(add-hook 'after-make-frame-functions 
          (lambda (frame)
            (select-frame frame)
            (my/setup-hl-line-for-transparent-bg)))

;; Make sure hl-line works in all buffers
(defun my/enable-hl-line-globally ()
  "Enable hl-line mode in all buffers."
  (global-hl-line-mode 1))

;; Apply after init
(add-hook 'after-init-hook 'my/enable-hl-line-globally)

;; Also enable for new frames
(defun my/enable-hl-line-for-new-frame (frame)
  "Enable hl-line mode for newly created FRAME."
  (with-selected-frame frame
    (global-hl-line-mode 1)))

(add-hook 'after-make-frame-functions 'my/enable-hl-line-for-new-frame)

;; Ensure hl-line is enabled in major modes
(defun my/ensure-hl-line-in-major-modes ()
  "Ensure hl-line is enabled in programming and text modes."
  (hl-line-mode 1))

;; Enable in common modes
(add-hook 'prog-mode-hook 'my/ensure-hl-line-in-major-modes)
(add-hook 'text-mode-hook 'my/ensure-hl-line-in-major-modes)
(add-hook 'org-mode-hook 'my/ensure-hl-line-in-major-modes)

(provide 'hl-line-config)
;;; hl-line-config.el ends here