;;; init-packages.el --- 包管理配置
;; 配置和安装额外包

(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm))

;; 配置 which-key
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-max-display-columns 3))


;; 配置更好的模式行
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-project-detection 'project))

;; 配置咨询工具
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("M-g e" . consult-recent-file)
         ("M-g f" . consult-find)
         ("M-g g" . consult-grep)
         ("M-g l" . consult-line)
         ("M-g m" . consult-mark)
         ("M-g w" . consult-imenu)))

;; 配置补全
(use-package corfu
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-cycle t
        corfu-quit-at-boundary t
        corfu-quit-no-match 'separator))

;; 配置编辑功能
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-count 10))

;; 配置项目管理
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'consult))

(provide 'init-packages)
;;; init-packages.el ends here
