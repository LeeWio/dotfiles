;;; corfu-config.el --- Corfu completion system configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Corfu completion system following official documentation and best practices

;;; Code:

;; Install and configure corfu following official recommendations
(use-package corfu
  :ensure t
  :hook (prog-mode . corfu-mode)
  :init
  (global-corfu-mode 1)
  :custom
  ;; Essential corfu settings from official documentation
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum prefix length for auto completion
  (corfu-auto-delay 0.1)         ;; Small delay before showing completions
  (corfu-separator ?\s)          ;; Separator for cycling
  (corfu-quit-at-boundary nil)   ;; Continue completion at buffer boundary
  (corfu-quit-no-match nil)      ;; Continue if there is no match
  (corfu-preview-current 'insert) ;; Preview current candidate
  (corfu-scroll-margin 5)        ;; Scroll margin for candidates
  (corfu-max-width 80)           ;; Maximum width of candidates
  (corfu-count 14)               ;; Number of candidates to show
  (corfu-on-exact-match 'quit)   ;; Quit on exact match
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . corfu-complete)
              ("M-d" . corfu-popupinfo-toggle)
              ("M-l" . corfu-show-location)))

;; Enable corfu popup information (built into corfu)
(with-eval-after-load 'corfu
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode))

;; Configure completion styles following official recommendations
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

;; Use Orderless for flexible completion filtering
(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

;; Ensure eglot works properly with corfu
;; No special configuration needed - corfu works with any completion-at-point backend
;; Eglot is built into Emacs 29+, so we just need to require it
(require 'eglot)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(provide 'corfu-config)
;;; corfu-config.el ends here