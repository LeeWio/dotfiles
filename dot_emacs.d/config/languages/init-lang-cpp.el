;;; init-lang-cpp.el --- C/C++ 语言支持配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: languages, c, cpp

;;; Commentary:

;; C/C++ 语言支持配置文件
;; 包含 LSP、clang-format 等配置

;;; Code:

;; C/C++ 基础配置
(use-package cc-mode
  :ensure nil
  :defer t
  :hook (c-mode-common . (lambda ()
                          (setq c-basic-offset 4)
                          (setq tab-width 4)
                          (setq indent-tabs-mode nil))))

;; LSP 支持
(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((c-mode c++-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-enable t)
  (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet t)  ; 启用代码片段
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable t)  ; 启用代码操作
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-enable-symbol-highlighting t)  ; 启用符号高亮
  (setq lsp-enable-on-type-formatting nil)  ; 关闭实时格式化
  (setq lsp-enable-text-document-color nil)
  (setq lsp-signature-auto-activate t)  ; 启用函数签名自动激活
  (setq lsp-signature-render-documentation t)
  (setq lsp-enable-file-watchers nil)  ; 提高性能，禁用文件监视
  (setq lsp-keep-workspace-alive nil)  ; 关闭时清理工作空间
  (setq lsp-completion-show-detail t)   ; 显示补全详情
  (setq lsp-completion-show-kind t))    ; 显示补全类型

;; C/C++ LSP 服务器配置 (clangd)
(use-package lsp-clangd
  :ensure lsp-mode
  :defer t
  :hook ((c-mode c++-mode) . lsp-deferred)
  :config
  (setq lsp-clients-clangd-args
        '("-j=4"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=iwyu"
          "--header-insertion-decorators=0"))
  ;; 设置 clangd 为 C/C++ 的默认客户端
  (add-to-list 'lsp-enabled-clients 'clangd))

;; clang-format 配置
(use-package clang-format
  :ensure t
  :defer t
  :bind (("C-c f" . clang-format-region)
         ("C-c F" . clang-format-buffer))
  :hook ((c-mode c++-mode) . (lambda ()
                              (add-hook 'before-save-hook 'clang-format-buffer nil t)))
  :config
  (setq clang-format-style "file")  ; 使用项目中的 .clang-format 文件
  (setq clang-format-fallback-style "Google"))  ; 如果没有 .clang-format 文件则使用 Google 风格

;; LSP UI 增强（可选）
(use-package lsp-ui
  :ensure t
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-sideline-enable t)  ; 启用侧边栏信息
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-flycheck-enable nil))

;; Company 模式（强大的补全框架）
(use-package company
  :ensure t
  :defer t
  :hook ((lsp-mode . company-mode))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-backends '((company-capf :with company-dabbrev-code company-keywords)))
  (setq company-tooltip-align-annotations t))  ; 对齐注解

;; Company Box（美化补全菜单）
(use-package company-box
  :ensure t
  :defer t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (setq company-box-doc-enable t))

(provide 'init-lang-cpp)

;;; init-lang-cpp.el ends here