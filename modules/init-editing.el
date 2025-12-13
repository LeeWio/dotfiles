;;; init-editing.el --- 编辑功能配置
;; 配置编辑相关功能

;; 智能缩进
(electric-indent-mode 1)

;; 删除末尾空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 括号匹配
(show-paren-mode 1)
(setq show-paren-delay 0)

;; 自动换行
(global-visual-line-mode 1)

;; 选择增强
(delete-selection-mode 1)
(transient-mark-mode 1)

;; 撤销增强
(setq undo-limit 80000000)
(setq undo-strong-limit 120000000)
(setq undo-outer-limit 150000000)

;; 拼写检查
(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")

;; 滚动设置
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

(provide 'init-editing)
;;; init-editing.el ends here
