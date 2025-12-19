;;; init-which-key.el --- Which-key 配置
;; Copyright (C) 2025 Wei Li
;; Author: Wei Li
;; Version: 1.0
;; Keywords: convenience, help

;;; Commentary:
;; Which-key 配置文件
;; 提供按键序列的实时提示功能

;;; Code:

;; Which-key 包配置
(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :config
  ;; 启用 which-key 模式
  (which-key-mode 1)
  
  ;; 基本设置
  (setq which-key-idle-delay 0.5)  ; 延迟 0.5 秒显示
  (setq which-key-popup-type 'side-window)  ; 在侧边窗口显示
  (setq which-key-side-window-location 'bottom)  ; 侧边窗口位置在底部
  
  ;; 设置弹窗大小
  (setq which-key-side-window-max-width 0.33)  ; 最大宽度为 1/3
  (setq which-key-side-window-max-height 0.25) ; 最大高度为 1/4
  
  ;; 启用图标支持（如果可用）
  (setq which-key-use-C-h-commands t)  ; 启用 C-h 命令
  
  ;; 自定义替换规则，使显示更友好
  (setq which-key-replacement-alist
        '(
          ;; 简化常见的前缀描述
          ("C-x" . "C-x")
          ("C-c" . "C-c")
          ("C-x 8" . "Unicode")
          ("C-x r" . "Registers")
          ("C-x v" . "VC")
          ("C-x 4" . "Other Window")
          ("C-x 5" . "Frame")
          ("C-x n" . "Narrow")
          ("C-x w" . "Window")
          ("C-x RET" . "TTY")
          
          ;; 简化模式相关的描述
          ("M-s" . "Search")
          ("M-o" . "Facemenu")
          ("M-g" . "Goto")
          ("M-h" . "Help")
          ("M-t" . "Transpose")
          
          ;; 简化函数名
          ("forward-sexp" . "fwrd-sexp")
          ("backward-sexp" . "back-sexp")
          ("forward-word" . "fwrd-word")
          ("backward-word" . "back-word")
          ("forward-char" . "fwrd-char")
          ("backward-char" . "back-char")
          ))
  
  ;; 设置排序方式
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  
  ;; 设置最大显示行数
  (setq which-key-max-description-length 25)
  
  ;; 启用分页（如果有大量按键）
  (setq which-key-allow-imprecise-window-fit t)
  
  ;; 设置帮助文本
  (setq which-key-help-text
        "按键帮助: C-h 获取更多信息, C-g 退出")
  
  ;; 启用分隔符
  (setq which-key-separator " → ")
  
  ;; 启用计数显示
  (setq which-key-show-remaining-keys t)
  
  ;; 自定义按键组
  (which-key-add-key-based-replacements
    "C-c C-l" "LSP"
    "C-c g" "Git"
    "C-c o" "Org"
    "C-x w" "Window"
    "C-x m" "Macro")
  
  ;; 为特定模式添加按键提示
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c C-l" "Link"
    "C-c C-x" "Export/Capture"
    "C-c C-a" "Archive")
  
  ;; 设置弹窗外观
  (setq which-key-special-keys
        '("SPC" "TAB" "RET" "ESC" "DEL"))
  
  ;; 启用动画效果（如果支持）
  (setq which-key-allow-evil-operators t)
  
  ;; 设置最小字符宽度
  (setq which-key-min-column-description-width 20)
  
  ;; 启用模糊匹配
  (setq which-key-enable-extended-define-key t)
  
  ;; 自定义外观设置以匹配 Catppuccin 主题
  (setq which-key-highlighted-command-face 'which-key-highlighted-command-face)
  (setq which-key-key-face 'which-key-key-face)
  (setq which-key-separator-face 'which-key-separator-face)
  (setq which-key-group-description-face 'which-key-group-description-face)
  (setq which-key-command-description-face 'which-key-command-description-face)
  
  ;; 设置透明背景以匹配主题
  (set-face-attribute 'which-key-key-face nil
                      :background nil
                      :foreground "#89b4fa")  ; Catppuccin blue
  (set-face-attribute 'which-key-separator-face nil
                      :background nil
                      :foreground "#cba6f7")  ; Catppuccin mauve
  (set-face-attribute 'which-key-command-description-face nil
                      :background nil
                      :foreground "#cdd6f4")  ; Catppuccin text
  (set-face-attribute 'which-key-group-description-face nil
                      :background nil
                      :foreground "#f38ba8")  ; Catppuccin red
  (set-face-attribute 'which-key-highlighted-command-face nil
                      :background "#313244"   ; Catppuccin surface0
                      :foreground "#a6e3a1")  ; Catppuccin green
                      
  ;; 设置弹窗背景透明
  (set-face-attribute 'which-key-popup-background-face nil
                      :background nil))

(provide 'init-which-key)
;;; init-which-key.el ends here