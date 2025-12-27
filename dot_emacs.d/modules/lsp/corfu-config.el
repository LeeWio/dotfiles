;;; corfu-config.el --- Corfu + Eglot modern completion -*- lexical-binding: t -*-

;;; Commentary:
;; Fully working configuration for GUI + terminal (emacs -nw)

;;; Code:

;; =========================
;; CORFU
;; =========================
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (setq completion-in-region-function #'corfu-completion-in-region)

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  (corfu-count 14)
  (corfu-scroll-margin 5)
  (corfu-preview-current 'insert)

  ;; ⭐️ 防止 “No match 卡死输入”
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-on-exact-match nil)

  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ;; 如果你发现 RET 被 corfu 接管，也可以取消绑定：
              ;; ("RET" . nil)
              ("RET" . corfu-complete)))


;; =========================
;; TERMINAL SUPPORT !!! 关键
;; =========================
(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (corfu-terminal-mode 1))


;; =========================
;; POPUPINFO
;; =========================
;; 这是内置模块，不能用 use-package 独立安装
(with-eval-after-load 'corfu
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1))

;; =========================
;; ORDERLESS
;; =========================
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles basic partial-completion)))))

;; =========================
;; YASNIPPET
;; =========================
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; =========================
;; ICONS
;; =========================
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  ;; 终端无图标不会崩
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; =========================
;; EGLOT
;; =========================
(require 'eglot)

(with-eval-after-load 'eglot
  ;; inlay hints (emacs29+)
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

  ;; ensure CAPF works w/ corfu
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions
                           #'eglot-completion-at-point))))

(use-package consult-eglot
  :ensure t
  :after eglot)

(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)

(provide 'corfu-config)
;;; corfu-config.el ends here