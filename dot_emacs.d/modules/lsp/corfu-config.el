;;; corfu-config.el --- Corfu completion system configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Corfu completion system with orderless filtering for enhanced programming experience

;;; Code:

;; Install and configure corfu
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  :custom
  ;; Configure corfu display
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Minimum prefix length for auto completion
  (corfu-auto-delay 0.1)         ;; Delay before showing completions
  (corfu-popupinfo-delay 0.2)    ;; Delay for popup info
  (corfu-preselect-first t)      ;; Preselect first candidate
  (corfu-on-exact-match 'quit)   ;; Quit on exact match
  (corfu-quit-at-boundary 'separator) ;; Quit at boundary
  (corfu-quit-no-match t)        ;; Quit if there is no match
  (corfu-separator ?\s)          ;; Separator for cycling
  (corfu-preview-current 'insert) ;; Preview current candidate
  (corfu-scroll-margin 5)        ;; Scroll margin for candidates
  (corfu-max-width 80)           ;; Maximum width of candidates
  (corfu-count 14)               ;; Number of candidates to show
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("M-d" . corfu-popupinfo-toggle)
              ("M-l" . corfu-show-location)))

;; Popup information for corfu (built into corfu since version 1.0)
(with-eval-after-load 'corfu
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode))

;; Use Orderless for flexible completion filtering
(use-package orderless
  :ensure t
  :custom
  ;; Configure orderless matching styles
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

;; Integrate Corfu with Cape for additional completion backends
(use-package cape
  :ensure t
  :after corfu
  :init
  ;; Add Cape completion backends to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;; Ensure eglot works with corfu
(use-package eglot
  :ensure t
  :config
  ;; Make sure eglot's completion function is properly integrated
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Add eglot completion backend
              (add-to-list 'completion-at-point-functions 
                           #'eglot-completion-at-point nil 'local))))

(provide 'corfu-config)
;;; corfu-config.el ends here