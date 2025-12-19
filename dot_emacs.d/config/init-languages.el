;;; init-languages.el --- 编程语言支持配置入口

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: languages

;;; Commentary:

;; 编程语言支持配置文件入口
;; 加载各种编程语言的配置模块

;;; Code:

;; 添加语言配置目录到加载路径
(add-to-list 'load-path "~/.emacs.d/config/languages")

;; C/C++ 语言支持
(require 'init-lang-cpp)

;; Python 语言支持
(require 'init-lang-python)

;; JavaScript/TypeScript 语言支持
(require 'init-lang-js)

;; Go 语言支持
(require 'init-lang-go)

(provide 'init-languages)

;;; init-languages.el ends here