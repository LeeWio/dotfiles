;;; init-ace-window.el --- Ace-window 配置
;; Copyright (C) 2025 Wei Li
;; Author: Wei Li
;; Version: 1.0
;; Keywords: convenience, windows

;;; Commentary:
;; Ace-window 配置文件
;; 提供快速窗口切换功能

;;; Code:

;; Ace-window 包配置
(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . ace-window)
         ("M-o" . ace-window))
  :config
  ;; 设置窗口标签字符
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  
  ;; 设置标签背景色以匹配 Catppuccin 主题
  (setq aw-background nil)
  
  ;; 启用单字符切换（只有一个窗口时）
  (setq aw-one-window t)
  
  ;; 设置标签外观
  (setq aw-leading-char-style 'char)
  
  ;; 启用边缘模式
  (setq aw-dispatch-always t)
  
  ;; 自定义标签外观
  (with-eval-after-load 'ace-window
    ;; 设置标签 face
    (set-face-attribute 'aw-leading-char-face nil
                        :foreground "#f38ba8"  ; Catppuccin red
                        :background "#313244"  ; Catppuccin surface0
                        :weight 'bold
                        :height 1.5)
    
    ;; 设置引导字符 face
    (set-face-attribute 'aw-key-face nil
                        :foreground "#89b4fa"  ; Catppuccin blue
                        :weight 'bold)
    
    ;; 设置背景 face
    (set-face-attribute 'aw-background-face nil
                        :background nil)
    
    ;; 设置模式行指示器
    (set-face-attribute 'aw-mode-line-face nil
                        :foreground "#a6e3a1"  ; Catppuccin green
                        :weight 'bold)))

(provide 'init-ace-window)
;;; init-ace-window.el ends here