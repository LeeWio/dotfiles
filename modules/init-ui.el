;;; init-ui.el --- 界面配置
;; 配置Emacs的外观和感觉

;; 启用行号
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; 设置缩进
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; 设置默认主题
;;(setq custom-enabled-themes '(wombat))
;;(setq-default cursor-type 'bar)

;; 高亮当前行
(global-hl-line-mode 1)

;; 显示列号
(setq column-number-mode t)

;; 简化标题栏
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                           (abbreviate-file-name (buffer-file-name))
                                         "%b"))))

;; 简化模式行
(setq mode-line-format
      '("%e"  ; 错误
        mode-line-front-space
        mode-line-buffer-identification
        "   "  ; 左右间距
        (:eval (when (buffer-file-name)
                 (format "📁 %s" (abbreviate-file-name (buffer-file-name)))))
        "   "
        "⎇ " (buffer-name)
        "   "
        (:eval (when (buffer-modified-p) "● "))
        "   "
        "Ln %l, Col %c"
        "   "
        (:eval (propertize (format "[%s]" (current-input-method-title))
                           'face 'mode-line-emphasis))))

(provide 'init-ui)
;;; init-ui.el ends here
