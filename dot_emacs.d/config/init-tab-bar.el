;;; init-tab-bar.el --- Tab Bar 配置
;; Copyright (C) 2025 Wei Li
;; Author: Wei Li
;; Version: 1.0
;; Keywords: convenience, tabs

;;; Commentary:
;; Tab Bar 配置文件
;; 提供类似 VSCode 的标签页功能

;;; Code:

;; 启用 Tab Bar 模式
(use-package tab-bar
  :ensure nil  ; 内置于 Emacs 27.1+
  :defer t
  :config
  ;; 启用 Tab Bar
  (tab-bar-mode 1)
  
  ;; 设置 Tab Bar 位置
  (setq tab-bar-position 'top)
  
  ;; 启用新 Tab 时显示按钮
  (setq tab-bar-show 1)
  
  ;; 设置 Tab 的关闭行为
  (setq tab-bar-close-button-show nil)
  
  ;; 设置 Tab 的新建按钮
  (setq tab-bar-new-button-show nil)
  
  ;; 启用 Tab 的自动命名
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  
  ;; 设置 Tab 名称截断长度
  (setq tab-bar-tab-name-truncated-max 15)
  
  ;; 启用 Tab 的缓冲区名称显示
  (setq tab-bar-tab-name-buffer-function 'tab-bar-tab-name-buffer)
  
  ;; 自定义 Tab 外观以匹配 Catppuccin 主题
  (with-eval-after-load 'tab-bar
    ;; 设置 Tab face
    (set-face-attribute 'tab-bar nil
                        :background "#181825"  ; Catppuccin mantle
                        :foreground "#cdd6f4"  ; Catppuccin text
                        :box nil
                        :height 0.9)
    
    ;; 设置选中 Tab face
    (set-face-attribute 'tab-bar-tab nil
                        :background "#313244"  ; Catppuccin surface0
                        :foreground "#89b4fa"  ; Catppuccin blue
                        :box nil
                        :weight 'bold
                        :height 0.9)
    
    ;; 设置未选中 Tab face
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :background "#181825"  ; Catppuccin mantle
                        :foreground "#7f849c"  ; Catppuccin subtext0
                        :box nil
                        :height 0.9)
    
    ;; 设置 Tab 分隔符
    (set-face-attribute 'tab-bar-tab-group-inactive nil
                        :background "#181825"  ; Catppuccin mantle
                        :foreground "#7f849c")  ; Catppuccin subtext0
    
    ;; 设置 Tab 组选中状态
    (set-face-attribute 'tab-bar-tab-group-current nil
                        :background "#313244"  ; Catppuccin surface0
                        :foreground "#89b4fa")) ; Catppuccin blue
  
  ;; 设置快捷键
  :bind (:map tab-bar-mode-map
         ("C-x t c" . tab-bar-mode)
         ("C-x t n" . tab-new)
         ("C-x t o" . tab-next)
         ("C-x t p" . tab-previous)
         ("C-x t k" . tab-close)
         ("C-x t r" . tab-rename)))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here