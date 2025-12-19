;;; init-hl-line.el --- Emacs 当前行高亮配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: highlighting

;;; Commentary:

;; 当前行高亮配置文件
;; 包含当前行高亮相关设置

;;; Code:

;; 高亮当前行
(global-hl-line-mode 1)

;; 设置当前行高亮的背景色为 Catppuccin Mocha 主题的浅色变体
(set-face-attribute 'hl-line nil
                   :background "#313244"
                   :foreground 'unspecified)

(provide 'init-hl-line)

;;; init-hl-line.el ends here