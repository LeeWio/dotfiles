;;; init-editing.el --- Emacs 编辑增强配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: editing

;;; Commentary:

;; 编辑功能增强配置文件
;; 包含文本编辑、代码编辑的相关设置

;;; Code:

;; 启用行号显示（兼容不同版本的 Emacs）
(when (fboundp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode 1))

;; 设置相对行号
(setq display-line-numbers-type 'relative)

;; 高亮当前行
(global-hl-line-mode 1)

;; 设置当前行高亮的背景色为 Catppuccin Mocha 主题的浅色变体
;; 使用更低调的颜色，与主题整体风格保持一致
(set-face-attribute 'hl-line nil
                   :background "#313244"
                   :foreground 'unspecified)

(provide 'init-editing)

;;; init-editing.el ends here