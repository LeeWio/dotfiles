;;; init-frame-title.el --- Emacs 标题栏配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: frame

;;; Commentary:

;; 标题栏配置文件
;; 包含框架标题格式设置

;;; Code:

;; 简化标题栏
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                           (abbreviate-file-name (buffer-file-name))
                                         "%b"))))

(provide 'init-frame-title)

;;; init-frame-title.el ends here