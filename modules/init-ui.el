;;; init-ui.el --- 界面配置
;; 配置Emacs的外观和感觉

;; 启用行号
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; 确保禁用菜单栏
(menu-bar-mode -1)

;; 设置缩进
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; 高亮当前行
(global-hl-line-mode 1)

;; 显示列号
(setq column-number-mode t)

;; 简化标题栏
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                           (abbreviate-file-name (buffer-file-name))
                                         "%b"))))

;; 平滑滚动配置
(pixel-scroll-precision-mode 1)  ;; Emacs 29+ 原生平滑滚动
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-interpolate-page t)

;; 鼠标滚动配置
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ;; 一次滚动一行
(setq mouse-wheel-progressive-speed nil)  ;; 不加速
(setq mouse-wheel-follow-mouse 't)  ;; 滚动鼠标所在窗口

;; 键盘滚动配置
(setq scroll-step 1)  ;; 键盘滚动一行
(setq scroll-margin 3)  ;; 光标距离顶部/底部保持3行
(setq scroll-conservatively 101)  ;; 大于100避免重新居中
(setq scroll-preserve-screen-position t)  ;; 滚动时保持光标屏幕位置

;; 禁用自动重新居中
(setq auto-window-vscroll nil)

;; 平滑翻页 - 使 C-v/M-v 也平滑滚动
(defun my/smooth-scroll-down ()
  "平滑向下滚动半屏。"
  (interactive)
  (let ((lines (/ (window-body-height) 2)))
    (dotimes (_ lines)
      (scroll-up 1)
      (sit-for 0.01))))

(defun my/smooth-scroll-up ()
  "平滑向上滚动半屏。"
  (interactive)
  (let ((lines (/ (window-body-height) 2)))
    (dotimes (_ lines)
      (scroll-down 1)
      (sit-for 0.01))))

;; 绑定到 C-v 和 M-v
(global-set-key (kbd "C-v") 'my/smooth-scroll-down)
(global-set-key (kbd "M-v") 'my/smooth-scroll-up)

;; mode-line 透明背景配置已移至 init-modeline.el
;; diff-hl 背景清除
(add-hook 'after-init-hook
          (lambda ()
            (when (facep 'diff-hl-change)
              (set-face-attribute 'diff-hl-change nil :background nil))
            (when (facep 'diff-hl-insert)
              (set-face-attribute 'diff-hl-insert nil :background nil))
            (when (facep 'diff-hl-delete)
              (set-face-attribute 'diff-hl-delete nil :background nil))))

(provide 'init-ui)
;;; init-ui.el ends here