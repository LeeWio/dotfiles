;;; catppuccin-config.el --- Catppuccin theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Catppuccin Mocha theme with transparent background

;;; Code:

;; Ensure package is initialized
(unless package--initialized
  (package-initialize))

;; Install catppuccin-theme if not already installed
(unless (package-installed-p 'catppuccin-theme)
  (package-refresh-contents)
  (package-install 'catppuccin-theme))

;; Load the theme
(load-theme 'catppuccin t)

;; Catppuccin customization
(setq catppuccin-flavor 'mocha) ; mocha, macchiato, frappe, latte
(setq catppuccin-opacity 1.0)   ; For transparent backgrounds

;; Define transparent background faces
(defun my/setup-catppuccin-transparent ()
  "Setup Catppuccin with transparent background and proper line numbering."
  ;; Ensure faces exist before setting attributes
  (when (facep 'default)
    (set-face-background 'default "transparent" nil))
  (when (facep 'fringe)
    (set-face-background 'fringe "transparent" nil))
  
  ;; Set semi-transparent backgrounds for UI elements (if faces exist)
  (when (facep 'header-line)
    (set-face-background 'header-line "#1e1e2e80" nil)) ; semi-transparent
  (when (facep 'mode-line)
    (set-face-background 'mode-line "#1e1e2e80" nil))   ; semi-transparent
  (when (facep 'mode-line-inactive)
    (set-face-background 'mode-line-inactive "#18182580" nil)) ; semi-transparent
  
  ;; Line numbers with transparency (if faces exist)
  (when (facep 'line-number)
    (set-face-background 'line-number "transparent" nil)
    (set-face-foreground 'line-number "#7f849c")) ; Catppuccin overlay1
  (when (facep 'line-number-current-line)
    (set-face-background 'line-number-current-line "#1e1e2e80" nil)
    (set-face-foreground 'line-number-current-line "#fab387")) ; Catppuccin peach
  
  ;; Relative line numbers
  (when (facep 'line-number-relative)
    (set-face-background 'line-number-relative "transparent" nil)
    (set-face-foreground 'line-number-relative "#6c7086")) ; Catppuccin overlay0
  
  ;; Region highlight (if face exists)
  (when (facep 'region)
    (set-face-background 'region "#585b70" nil)) ; Catppuccin surface2
  
  ;; Make minibuffer background transparent (if face exists)
  (when (facep 'minibuffer-prompt)
    (set-face-background 'minibuffer-prompt "transparent" nil)))

;; Apply transparent setup after theme is loaded
(with-eval-after-load 'catppuccin-theme
  (my/setup-catppuccin-transparent))

;; Reapply on frame creation for new frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my/setup-catppuccin-transparent)))

(provide 'catppuccin-config)
;;; catppuccin-config.el ends here