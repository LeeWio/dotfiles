;;; init-git.el --- Emacs Git 集成配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: git

;;; Commentary:

;; Git 集成配置文件
;; 包含 Magit 等 Git 相关工具配置

;;; Code:

;; Magit - 强大的 Git 界面
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch)
         ("C-c g s" . magit-status)
         ("C-c g d" . magit-diff-unstaged)
         ("C-c g c" . magit-commit)
         ("C-c g p" . magit-push)
         ("C-c g f" . magit-log-buffer-file))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

;; Git gutter - 在行号旁边显示 Git 状态
(use-package git-gutter
  :ensure t
  :defer t
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 0.05)
  (setq git-gutter:modified-sign "┃")
  (setq git-gutter:added-sign "┃")
  (setq git-gutter:deleted-sign "┃")
  (setq git-gutter:unchanged-sign " ")
  (setq git-gutter:hide-gutter-if-no-changes t)
  
  ;; 配置 Git gutter 颜色以匹配 Catppuccin Mocha 主题
  ;; 使用更低调的颜色，避免过于鲜艳
  (set-face-foreground 'git-gutter:modified "#89b4fa") ; Catppuccin blue
  (set-face-foreground 'git-gutter:added "#a6e3a1")    ; Catppuccin green
  (set-face-foreground 'git-gutter:deleted "#f38ba8")  ; Catppuccin red
  
  ;; 增强 Git gutter 显示效果
  (setq git-gutter:verbosity 0)) ; 减少输出信息

(provide 'init-git)

;;; init-git.el ends here