;;; init-rainbow-delimiters.el --- Rainbow delimiters 配置
;; Copyright (C) 2025 Wei Li
;; Author: Wei Li
;; Version: 1.0
;; Keywords: faces, convenience

;;; Commentary:
;; Rainbow delimiters 配置文件
;; 为 Lisp 系列语言提供彩虹括号支持

;;; Code:

;; Rainbow delimiters 包配置
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (ielm-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (hy-mode . rainbow-delimiters-mode))
  :config
  ;; 启用不同的分隔符类型高亮
  (setq rainbow-delimiters-max-face-count 8)
  
  ;; 启用括号不匹配高亮
  (setq rainbow-delimiters-unmatched-face
        'rainbow-delimiters-unmatched-face)
  
  ;; 启用括号匹配高亮
  (setq rainbow-delimiters-mismatched-face
        'rainbow-delimiters-mismatched-face)
  
  ;; 设置不同层级括号的外观
  (with-eval-after-load 'rainbow-delimiters
    ;; 第一层括号 (最外层)
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground "#89b4fa")  ; Catppuccin blue
    
    ;; 第二层括号
    (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                        :foreground "#cba6f7")  ; Catppuccin mauve
    
    ;; 第三层括号
    (set-face-attribute 'rainbow-delimiters-depth-3-face nil
                        :foreground "#fab387")  ; Catppuccin peach
    
    ;; 第四层括号
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil
                        :foreground "#f9e2af")  ; Catppuccin yellow
    
    ;; 第五层括号
    (set-face-attribute 'rainbow-delimiters-depth-5-face nil
                        :foreground "#a6e3a1")  ; Catppuccin green
    
    ;; 第六层括号
    (set-face-attribute 'rainbow-delimiters-depth-6-face nil
                        :foreground "#74c7ec")  ; Catppuccin sky
    
    ;; 第七层括号
    (set-face-attribute 'rainbow-delimiters-depth-7-face nil
                        :foreground "#f5c2e7")  ; Catppuccin pink
    
    ;; 第八层括号
    (set-face-attribute 'rainbow-delimiters-depth-8-face nil
                        :foreground "#b4befe")  ; Catppuccin lavender
    
    ;; 不匹配括号
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground "#f38ba8"   ; Catppuccin red
                        :weight 'bold
                        :underline t)
    
    ;; 不对应括号
    (set-face-attribute 'rainbow-delimiters-mismatched-face nil
                        :foreground "#f38ba8"   ; Catppuccin red
                        :background "#313244"   ; Catppuccin surface0
                        :weight 'bold)
    
    ;; 括号周围空白高亮
    (set-face-attribute 'rainbow-delimiters-whitespace-face nil
                        :background nil)
    
    ;; 基础分隔符高亮
    (set-face-attribute 'rainbow-delimiters-base-face nil
                        :foreground "#cdd6f4"))  ; Catppuccin text
    
  ;; 启用基偶数层不同的亮度
  (setq rainbow-delimiters-outermost-only-face-count 1)
  
  ;; 启用透明背景
  (setq rainbow-delimiters-highlight-parens-p t)
  (setq rainbow-delimiters-highlight-opening-parentheses-p t))

(provide 'init-rainbow-delimiters)
;;; init-rainbow-delimiters.el ends here