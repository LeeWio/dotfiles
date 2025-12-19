;;; init-ui.el --- Emacs 界面配置入口

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: ui

;;; Commentary:

;; 用户界面配置入口文件
;; 加载各种界面相关的配置模块

;;; Code:

;; 添加配置目录到加载路径
(add-to-list 'load-path "~/.emacs.d/config")

;; 行号配置
(require 'init-linenumbers)

;; 当前行高亮配置
(require 'init-hl-line)

;; 界面元素配置（菜单栏、工具栏、滚动条）
(require 'init-interface)

;; 光标配置
(require 'init-cursor)

;; 标题栏配置
(require 'init-frame-title)

;; 状态栏配置
(require 'init-modeline)

;; 缩进配置
(require 'init-indentation)

;; 列号配置
(require 'init-column-number)

;; diff-hl 背景清除
(add-hook 'after-init-hook
          (lambda ()
            (when (facep 'diff-hl-change)
              (set-face-attribute 'diff-hl-change nil :background nil))
            (when (facep 'diff-hl-insert)
              (set-face-attribute 'diff-hl-insert nil :background nil))
            (when (facep 'diff-hl-delete)
              (set-face-attribute 'diff-hl-delete nil :background nil))))

(provide 'init-ui)

;;; init-ui.el ends here