;;; init-vars.el --- 全局变量设置

;; 编码设置
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; 备份和自动保存设置
(setq backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save" user-emacs-directory) t)))
(make-directory (expand-file-name "backup" user-emacs-directory) t)
(make-directory (expand-file-name "auto-save" user-emacs-directory) t)

;; 文件行为
(setq confirm-kill-processes nil)
(setq create-lockfiles nil)
(setq make-backup-files t)
(setq backup-by-copying t)  ;; 通过复制而非重命名备份
(setq delete-old-versions t)  ;; 自动删除旧备份
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; 搜索和替换
(setq search-whitespace-regexp ".*?")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace t)

;; 性能优化
(setq auto-window-vscroll nil)  ;; 禁用自动垂直滚动优化
(setq fast-but-imprecise-scrolling t)  ;; 快速但不精确的滚动
(setq jit-lock-defer-time 0)  ;; 即时语法高亮
(setq redisplay-skip-fontification-on-input t)  ;; 输入时跳过字体化

(provide 'init-vars)
;;; init-vars.el ends here