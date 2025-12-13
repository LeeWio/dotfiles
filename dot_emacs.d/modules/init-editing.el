;;; init-editing.el --- 编辑功能配置
;; 配置编辑相关功能

;; 智能缩进
(electric-indent-mode 1)

;; 删除末尾空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 括号匹配
(show-paren-mode 1)
(setq show-paren-delay 0)
;; 优化括号匹配样式
(setq show-paren-style 'mixed)  ; 使用混合样式
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)

;; 自定义括号匹配面属性
(with-eval-after-load 'paren
  (set-face-attribute 'show-paren-match nil
                      :background "#45475a"
                      :foreground "#cdd6f4"
                      :weight 'bold
                      :box '(:line-width 1 :color "#89b4fa"))
  (set-face-attribute 'show-paren-mismatch nil
                      :background "#f38ba8"
                      :foreground "#1e1e2e"
                      :weight 'bold))

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