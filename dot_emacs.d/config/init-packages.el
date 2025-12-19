;;; init-packages.el --- Emacs 包管理配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: packages

;;; Commentary:

;; 包管理配置文件
;; 包含包管理器和插件的配置

;;; Code:

;; 初始化包管理器
(require 'package)
(setq package-enable-at-startup nil)

;; 添加包仓库
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

;; 初始化包系统
(package-initialize)

;; 确保 use-package 已安装
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; 配置 use-package
(setq use-package-always-ensure t)
(setq use-package-always-demand t)
(setq use-package-expand-minimally t)
(setq use-package-compute-statistics t)

;; 确保在加载包之前设置主题变量
(setq catppuccin-flavor 'mocha)

;; 安装并配置 Catppuccin Mocha 主题
(use-package catppuccin-theme
  :ensure t
  :demand t
  :config
  (load-theme 'catppuccin t))

;; 安装 posframe 和 async 包
(use-package posframe
  :ensure t
  :defer t)

(use-package async
  :ensure t
  :defer t)

;; 安装核心开发工具包
(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((prog-mode . lsp-deferred))
  :config
  ;; 配置诊断显示
  (setq lsp-javascript-display-diagnostic-source t)
  (setq lsp-typescript-display-diagnostic-source t)
  ;; 确保加载 JavaScript/TypeScript LSP 支持
  (require 'lsp-javascript))

;; 在 lsp-mode 加载后配置 Emacs Lisp 支持
(with-eval-after-load 'lsp-mode
  ;; 禁用对 Emacs Lisp 模式的 LSP 支持以避免警告
  (add-to-list 'lsp-disabled-clients 'emacs-lisp-mode))

(use-package lsp-ui
  :ensure t
  :defer t
  :hook (lsp-mode . lsp-ui-mode))

(use-package company
  :ensure t
  :defer t
  :hook (prog-mode . company-mode))

;; Blamer - 行级 Git 提交信息显示 (类似 VS Code GitLens)
(use-package blamer
  :load-path "~/.emacs.d/config"
  :defer t
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-type 'both)
  (blamer-view 'overlay)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

;; 确保补全相关包可用
(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode))

(use-package orderless
  :ensure t
  :defer t
  :config
  (setq completion-styles '(orderless)))

(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . global-corfu-mode))

;; DOOM Modeline
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-major-mode-icon t))

(provide 'init-packages)

;;; init-packages.el ends here