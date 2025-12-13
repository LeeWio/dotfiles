;;; init-vars.el --- 全局变量设置
;; 设置通用变量，不依赖其他包

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
(setq auto-save-default t)

;; 搜索和替换
(setq search-whitespace-regexp ".*?")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace t)

(provide 'init-vars)
;;; init-vars.el ends here
