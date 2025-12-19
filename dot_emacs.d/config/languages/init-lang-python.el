;;; init-lang-python.el --- Python 语言支持配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: languages, python

;;; Commentary:

;; Python 语言支持配置文件
;; 包含基础配置

;;; Code:

;; Python 模式配置
(use-package python
  :ensure nil
  :defer t
  :hook (python-mode . (lambda ()
                        (setq indent-tabs-mode nil)
                        (setq tab-width 4)
                        (setq python-indent-offset 4))))

(provide 'init-lang-python)

;;; init-lang-python.el ends here