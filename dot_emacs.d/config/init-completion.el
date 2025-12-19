;;; init-completion.el --- Emacs 补全系统配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: completion

;;; Commentary:

;; 补全系统配置文件
;; 包含 Corfu、Orderless、Vertico 等现代补全工具

;;; Code:

;; Vertico - 垂直补全界面
(use-package vertico
  :ensure t
  :defer t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-count 15))

;; Orderless - 模糊匹配补全样式
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

;; Corfu - 弹出式补全
(use-package corfu
  :ensure t
  :defer t
  :hook (after-init . global-corfu-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.1)
  (setq corfu-echo-documentation t)
  (setq corfu-scroll-margin 5)
  (setq corfu-count 15))

;; 设置补全弹窗的透明背景
(defun my-setup-transparent-completion ()
  "设置补全界面的透明背景。"
  ;; 使用条件设置避免错误
  (when (facep 'corfu-default)
    (set-face-attribute 'corfu-default nil :background nil))
  (when (facep 'corfu-current)
    (set-face-attribute 'corfu-current nil :background nil))
  (when (facep 'corfu-bar)
    (set-face-attribute 'corfu-bar nil :background nil))
  (when (facep 'corfu-border)
    (set-face-attribute 'corfu-border nil :background nil))
  (when (facep 'vertico-current)
    (set-face-attribute 'vertico-current nil :background nil)))

;; 在相关包加载后设置透明背景
(with-eval-after-load 'corfu
  (run-with-idle-timer 0.1 nil #'my-setup-transparent-completion))

(with-eval-after-load 'vertico
  (run-with-idle-timer 0.1 nil #'my-setup-transparent-completion))

(provide 'init-completion)

;;; init-completion.el ends here