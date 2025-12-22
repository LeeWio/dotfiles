;;; keybindings-init.el --- Comprehensive keybindings configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive keybindings configuration for improved Emacs productivity

;;; Code:

;; Install required packages
(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-char-timer)
         ("C-c j" . avy-goto-line)))

;; Install expand-region for progressive selection
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; MWIM is built into Emacs 29+
(unless (featurep 'mwim)
  (autoload 'mwim-beginning-of-line "mwim" nil t)
  (autoload 'mwim-end-of-line "mwim" nil t))

;; ==================== CUSTOM PREFIX MAP ====================

;; Create a prefix map for custom keybindings
(defvar my-keys-map (make-sparse-keymap)
  "Keymap for custom keybindings.")

;; Define the prefix key for custom keybindings
(global-set-key (kbd "C-c k") my-keys-map)

;; ==================== NAVIGATION & BUFFER MANAGEMENT ====================

;; Buffer navigation
(define-key my-keys-map (kbd "b n") 'next-buffer)
(define-key my-keys-map (kbd "b p") 'previous-buffer)
(define-key my-keys-map (kbd "b k") 'kill-this-buffer)
(define-key my-keys-map (kbd "b s") 'switch-to-buffer)
(define-key my-keys-map (kbd "b l") 'ibuffer)

;; Window navigation (also set globally for convenience)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; ==================== FILE & PROJECT MANAGEMENT ====================

;; File operations
(define-key my-keys-map (kbd "f f") 'find-file)
(define-key my-keys-map (kbd "f s") 'save-buffer)
(define-key my-keys-map (kbd "f w") 'write-file)
(define-key my-keys-map (kbd "f r") 'recentf-open-files)

;; Project management (if projectile is available)
(when (featurep 'projectile)
  (define-key my-keys-map (kbd "p f") 'projectile-find-file)
  (define-key my-keys-map (kbd "p p") 'projectile-switch-project)
  (define-key my-keys-map (kbd "p s") 'projectile-grep))

;; ==================== EDITING & TEXT MANIPULATION ====================

;; Text manipulation
(define-key my-keys-map (kbd "t c") 'capitalize-dwim)
(define-key my-keys-map (kbd "t l") 'downcase-dwim)
(define-key my-keys-map (kbd "t u") 'upcase-dwim)
(define-key my-keys-map (kbd "t r") 'reverse-region)
(define-key my-keys-map (kbd "t f") 'fill-paragraph)

;; Line operations
(define-key my-keys-map (kbd "l d") 'duplicate-dwim)
(define-key my-keys-map (kbd "l c") 'delete-trailing-whitespace)
(define-key my-keys-map (kbd "l n") 'move-to-column)
(define-key my-keys-map (kbd "l s") 'sort-lines)

;; ==================== PROGRAMMING-SPECIFIC ====================

;; Compilation and building
(define-key my-keys-map (kbd "c c") 'compile)
(define-key my-keys-map (kbd "c r") 'recompile)

;; Debugging
(define-key my-keys-map (kbd "d d") 'gdb)
(define-key my-keys-map (kbd "d t") 'toggle-debug-on-error)

;; ==================== WINDOW & UI MANAGEMENT ====================

;; Window operations
(define-key my-keys-map (kbd "w s") 'split-window-below)
(define-key my-keys-map (kbd "w v") 'split-window-right)
(define-key my-keys-map (kbd "w c") 'delete-window)
(define-key my-keys-map (kbd "w o") 'delete-other-windows)

;; UI toggles
(define-key my-keys-map (kbd "u l") 'global-display-line-numbers-mode)
(define-key my-keys-map (kbd "u h") 'global-hl-line-mode)
(define-key my-keys-map (kbd "u w") 'whitespace-mode)
(define-key my-keys-map (kbd "u f") 'auto-fill-mode)

;; ==================== MISCELLANEOUS ====================

;; Help and documentation
(define-key my-keys-map (kbd "h k") 'describe-key)
(define-key my-keys-map (kbd "h f") 'describe-function)
(define-key my-keys-map (kbd "h v") 'describe-variable)
(define-key my-keys-map (kbd "h m") 'describe-mode)

;; Search and replace
(define-key my-keys-map (kbd "s s") 'isearch-forward)
(define-key my-keys-map (kbd "s r") 'query-replace)
(define-key my-keys-map (kbd "s f") 'find-grep)

;; ==================== GLOBAL KEYBINDINGS ====================

;; Quick access to common functions
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c n") 'forward-paragraph)
(global-set-key (kbd "C-c p") 'backward-paragraph)

;; Enhanced navigation with mwim
(global-set-key (kbd "C-a") 'mwim-beginning-of-line)
(global-set-key (kbd "C-e") 'mwim-end-of-line)

;; Quick save
(global-set-key (kbd "C-x C-s") 'save-buffer)

;; ==================== TEXT SELECTION ====================

;; Selection helpers
(defun my/select-line ()
  "Select current line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

(defun my/select-paragraph ()
  "Select current paragraph."
  (interactive)
  (mark-paragraph)
  (exchange-point-and-mark))

(defun my/select-function ()
  "Select current function (if supported by mode)."
  (interactive)
  (mark-defun)
  (exchange-point-and-mark))

;; Bind selection helpers to global keys
(global-set-key (kbd "C-c s l") 'my/select-line)
(global-set-key (kbd "C-c s p") 'my/select-paragraph)
(global-set-key (kbd "C-c s f") 'my/select-function)

(provide 'keybindings-init)
;;; keybindings-init.el ends here