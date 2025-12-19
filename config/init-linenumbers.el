;;; init-linenumbers.el --- Emacs 行号配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: linenumbers

;;; Commentary:

;; 行号配置文件
;; 包含行号显示相关设置

;;; Code:

;; 启用行号显示
(global-display-line-numbers-mode 1)

;; 设置相对行号
(setq display-line-numbers-type 'relative)

(provide 'init-linenumbers)

;;; init-linenumbers.el ends here