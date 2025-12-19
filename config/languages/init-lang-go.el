;;; init-lang-go.el --- Go 语言支持配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: languages, go

;;; Commentary:

;; Go 语言支持配置文件
;; 包含 LSP、代码格式化等配置

;;; Code:

;; Go 模式配置
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :defer t
  :hook (go-mode . (lambda ()
                    (setq tab-width 4)
                    (setq indent-tabs-mode t))))

;; Go LSP 支持 (gopls)
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (go-mode . lsp-deferred))

(use-package lsp-go
  :ensure lsp-mode
  :defer t
  :hook (go-mode . lsp-deferred)
  :config
  (setq lsp-go-gofumpt t)
  (setq lsp-go-use-placeholders t))

;; Go 代码格式化
(use-package gofmt
  :ensure nil
  :defer t
  :hook (before-save . gofmt-before-save))

(provide 'init-lang-go)

;;; init-lang-go.el ends here