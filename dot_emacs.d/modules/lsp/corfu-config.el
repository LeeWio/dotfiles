;;; corfu-config.el --- Corfu completion system configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Corfu completion system with proper eglot integration

;;; Code:

;; Install and configure corfu
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  :config
  ;; Essential corfu settings
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 1)
  (setq corfu-auto-delay 0.1)
  (setq corfu-separator ?\s)
  (setq corfu-quit-at-boundary nil)
  (setq corfu-quit-no-match nil)
  (setq corfu-preview-current 'insert)
  (setq corfu-scroll-margin 5)
  (setq corfu-max-width 80)
  (setq corfu-count 14)
  (setq corfu-on-exact-match 'quit)
  
  ;; Key bindings
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . corfu-complete)
              ("M-d" . corfu-popupinfo-toggle)
              ("M-l" . corfu-show-location)))

;; Enable corfu popup information
(with-eval-after-load 'corfu
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode))

;; Configure completion styles
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; Use Orderless for flexible completion filtering
(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

;; Explicitly require eglot to ensure it's available
(require 'eglot)

;; Ensure eglot is properly integrated with corfu
;; We define this after corfu is loaded to ensure proper integration
(with-eval-after-load 'eglot
  ;; Make sure eglot's completion function is available
  (defun my-eglot-setup ()
    "Setup eglot completion with corfu."
    ;; Add eglot completion to the front of completion functions
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (add-to-list 'completion-at-point-functions 
                             #'eglot-completion-at-point nil t))))
  
  ;; Apply the setup
  (my-eglot-setup))

;; Enable eglot for C/C++ modes
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(provide 'corfu-config)
;;; corfu-config.el ends here