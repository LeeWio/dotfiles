;;; init-interface.el --- Emacs 界面元素配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: interface

;;; Commentary:

;; 界面元素配置文件
;; 包含菜单栏、工具栏、滚动条等界面元素的隐藏设置

;;; Code:

;; 确保禁用菜单栏
(menu-bar-mode -1)

;; 禁用工具栏（兼容不同版本的 Emacs）
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; 禁用滚动条（兼容不同版本的 Emacs）
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; 另一种禁用滚动条的方法（通过参数设置）
(setq default-frame-alist '((vertical-scroll-bars . nil)
                           (horizontal-scroll-bars . nil)))

(provide 'init-interface)

;;; init-interface.el ends here