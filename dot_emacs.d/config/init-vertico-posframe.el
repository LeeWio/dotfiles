;;; init-vertico-posframe.el --- Vertico Posframe 配置
;; Copyright (C) 2025 Wei Li
;; Author: Wei Li
;; Version: 1.0
;; Keywords: convenience, completion

;;; Commentary:
;; Vertico Posframe 配置文件
;; 为 M-x 等命令提供居中弹窗效果

;;; Code:

;; Vertico Posframe 包配置
(use-package vertico-posframe
  :ensure t
  :defer t
  :after vertico
  :config
  ;; 启用 Vertico Posframe 模式
  (vertico-posframe-mode 1)
  
  ;; 设置弹窗参数以实现居中显示
  (setq vertico-posframe-parameters
        '((left . 0.5)
          (top . 0.5)
          (width . 0.5)
          (height . 0.3)
          (min-width . 80)
          (min-height . 10)
          (internal-border-width . 2)))
  
  ;; 使用居中位置处理器
  (setq vertico-posframe-poshandler 
        #'posframe-poshandler-frame-center)
  
  ;; 为特定命令设置弹窗样式
  (setq vertico-multiform-commands
        '((execute-extended-command . posframe)
          (switch-to-buffer . posframe)
          (find-file . posframe)
          (t . posframe)))
  
  ;; 自定义弹窗外观以匹配 Catppuccin 主题
  (with-eval-after-load 'vertico-posframe
    ;; 设置弹窗边框
    (set-face-attribute 'vertico-posframe-border nil
                        :background "#313244")  ; Catppuccin surface0
    
    ;; 设置弹窗内部背景
    (set-face-attribute 'vertico-posframe nil
                        :background "#1e1e2e"   ; Catppuccin base
                        :foreground "#cdd6f4"   ; Catppuccin text
                        :inherit 'default)
    
    ;; 设置选中项背景
    (set-face-attribute 'vertico-current nil
                        :background "#313244"   ; Catppuccin surface0
                        :foreground "#89b4fa"   ; Catppuccin blue
                        :weight 'bold)))

(provide 'init-vertico-posframe)
;;; init-vertico-posframe.el ends here