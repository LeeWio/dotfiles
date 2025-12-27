;;; vertico-config.el --- Vertico, Orderless, Consult configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Vertico + Orderless + Consult modern completion system configuration

;;; Code:

;; Install packages if not already installed
(unless (package-installed-p 'vertico)
  (package-refresh-contents)
  (package-install 'vertico))

(unless (package-installed-p 'consult)
  (package-refresh-contents)
  (package-install 'consult))

;; Load packages
(require 'vertico)
(require 'consult)

;; Enable vertico globally
(vertico-mode 1)

;; Configure orderless with additional settings (assuming already loaded in corfu-config.el)
(with-eval-after-load 'orderless
  ;; Configure orderless with flex matching as fallback
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-matching-styles '(orderless-prefixes orderless-initialism orderless-flex)))

;; Configure vertico
(setq vertico-count 10)                    ; Number of candidates to show
(setq vertico-cycle t)                     ; Cycle from last to first candidate
(setq vertico-resize t)                    ; Resize the minibuffer to fit candidates
(setq vertico-scroll-margin 2)             ; How many lines before scrolling

;; Integrate with savehist to persist history
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; Key bindings for vertico
(with-eval-after-load 'vertico
  :ensure nil
  ;; Use C-j and C-k to cycle through candidates
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  ;; Use C-v and M-v for page up/down
  (define-key vertico-map (kbd "C-v") #'scroll-up-command)
  (define-key vertico-map (kbd "M-v") #'scroll-down-command)
  ;; Use C-M-n and C-M-p for history navigation
  (define-key vertico-map (kbd "C-M-n") #'vertico-next-group)
  (define-key vertico-map (kbd "C-M-p") #'vertico-previous-group))

;; Configure Consult for better search and navigation
(setq consult-narrow-key "<")             ; Use < to narrow in consult search
(setq consult-project-function 
      (lambda () (projectile-project-root)))  ; Use projectile for project root

;; Enable consult preview for some commands
(setq consult-preview-key 'any)
(setq consult-async-min-input 2)
(setq consult-async-refresh-delay  0.15)
(setq consult-async-input-throttle 0.2)
(setq consult-async-input-debounce 0.1)

;; Use `consult-completion-in-region' if vertico is enabled
;; otherwise use the default `completion--in-region' which is the default
(advice-add 'completion-in-region :override #'consult-completion-in-region)

;; Configure some useful consult commands
(with-eval-after-load 'consult
  ;; Use consult to get the completion history
  (define-key minibuffer-local-map (kbd "C-r") #'consult-history)
  (define-key minibuffer-local-map (kbd "C-l") #'consult-line)
  (define-key minibuffer-local-map (kbd "C-k") #'consult-keep-lines)
  (define-key minibuffer-local-map (kbd "C-u") #'consult-focus-lines))

;; Add keymap for useful consult commands
(define-key global-map (kbd "C-x b") #'consult-buffer)        ; Quick switch buffer
(define-key global-map (kbd "C-x 4 b") #'consult-buffer-other-window) ; Switch buffer in other window
(define-key global-map (kbd "C-x 5 b") #'consult-buffer-other-frame)  ; Switch buffer in other frame
(define-key global-map (kbd "C-c h") #'consult-history)       ; Search in history
(define-key global-map (kbd "C-c m") #'consult-mode-command)  ; Execute mode command
(define-key global-map (kbd "C-c k") #'consult-kmacro)        ; Keyboard macro consult
(define-key global-map (kbd "C-M-j") #'consult-line)          ; Jump to line in current buffer
(define-key global-map (kbd "C-M-g") #'consult-ripgrep)       ; Search with ripgrep
(define-key global-map (kbd "C-c g") #'consult-git-grep)      ; Search with git grep
(define-key global-map (kbd "C-c f") #'consult-find)          ; Find files
(define-key global-map (kbd "M-y") #'consult-yank-pop)        ; Replace yank with consult yank
(define-key global-map (kbd "M-s e") #'consult-isearch-history) ; Search isearch history

;; Use consult for some built-in functions
(consult-customize
  consult-theme
  :preview-key '(:debounce 0.2 any)
  consult-ripgrep consult-git-grep consult-grep
  consult-bookmark consult-recent-file consult-xref
  consult--source-bookmark consult--source-file-register
  consult--source-recent-file consult--source-project-recent-file
  ;; :preview-key "M-."
  :preview-key '(:debounce 0.4 any))

;; Minibuffer history
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(provide 'vertico-config)
;;; vertico-config.el ends here