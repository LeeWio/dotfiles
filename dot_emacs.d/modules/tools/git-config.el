;;; git-config.el --- Git tools configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for Git-related tools including Magit and Git Gutter

;;; Code:

;; ==================== Magit - Git interface ====================
(use-package magit
  :ensure t
  :defer t
  :bind (("C-c g s" . magit-status)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log)
         ("C-c g f" . magit-file-dispatch)
         ("C-c g d" . magit-diff-unstaged)
         ("C-c g c" . magit-commit)
         ("C-c g p" . magit-push)
         ("C-c g P" . magit-pull))
  :config
  ;; Performance optimizations
  (setq magit-refresh-status-buffer nil)
  (setq magit-auto-revert-mode-lighter "")
  (setq magit-process-popup-time 10))

;; ==================== Git Gutter - 在行号旁显示Git状态 ====================
(use-package git-gutter
  :ensure t
  :defer t
  :diminish git-gutter-mode
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :bind (("C-c g >" . git-gutter:next-hunk)
         ("C-c g <" . git-gutter:previous-hunk)
         ("C-c g r" . git-gutter:revert-hunk))
  :config
  ;; Visual customizations
  (setq git-gutter:update-interval 0.02)
  (setq git-gutter:lighter " GG")
  (setq git-gutter:modified-sign "┃")
  (setq git-gutter:added-sign "┃")
  (setq git-gutter:deleted-sign "┃")
  
  ;; Enable global git-gutter mode
  (global-git-gutter-mode 1))

;; ==================== Browse at remote - 在浏览器中查看Git文件 ====================
(use-package browse-at-remote
  :ensure t
  :defer t
  :bind (("C-c g o" . browse-at-remote)))

(provide 'git-config)
;;; git-config.el ends here