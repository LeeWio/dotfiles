;;; init-lang-js.el --- JavaScript/TypeScript 语言支持配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: languages, javascript, typescript

;;; Commentary:

;; JavaScript/TypeScript 语言支持配置文件
;; 包含 LSP、代码格式化等配置

;;; Code:

;; JavaScript 模式配置
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :defer t
  :config
  (setq js2-basic-offset 2)
  (setq js2-bounce-indent-p nil))

;; TypeScript 模式配置
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :defer t
  :config
  (setq typescript-indent-level 2))

;; JSON 模式
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :defer t)

;; JavaScript/TypeScript LSP 支持
(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((js-mode js2-mode typescript-mode) . lsp-deferred)
  :commands lsp
  :init
  (setq lsp-prefer-flymake nil)
  :config
  ;; 配置诊断显示
  (setq lsp-javascript-display-diagnostic-source t)
  (setq lsp-typescript-display-diagnostic-source t)
  ;; 确保加载 JavaScript/TypeScript LSP 支持
  (require 'lsp-javascript))

;; JavaScript/TypeScript 补全增强
(use-package company
  :ensure t
  :defer t
  :hook ((js-mode js2-mode typescript-mode) . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-backends '((company-capf :with company-dabbrev-code company-keywords)))
  (setq company-tooltip-align-annotations t))

;; LSP UI 增强
(use-package lsp-ui
  :ensure t
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-flycheck-enable nil))

(provide 'init-lang-js)

;;; init-lang-js.el ends here