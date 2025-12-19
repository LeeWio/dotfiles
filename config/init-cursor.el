;;; init-cursor.el --- Emacs 光标配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: cursor

;;; Commentary:

;; 光标配置文件
;; 包含光标样式和颜色设置

;;; Code:

;; 设置光标颜色
(setq cursor-type 'bar)
(setq cursor-in-non-selected-windows 'box)

(provide 'init-cursor)

;;; init-cursor.el ends here