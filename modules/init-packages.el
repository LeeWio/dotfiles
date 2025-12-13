;;; init-packages.el --- 包管理配置

;; ====== 主题配置 ======
(use-package catppuccin-theme
  :ensure t
  :demand t
  :init
  (setq catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :no-confirm))

;; ====== UI 增强 ======
(use-package which-key
  :ensure t
  :demand t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-max-display-columns 3))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; 使用 Catppuccin 配色方案优化 rainbow-delimiters
  (with-eval-after-load 'rainbow-delimiters
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#f38ba8")   ; pink
    (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#fab387")   ; peach
    (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#f9e2af")   ; yellow
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#a6e3a1")   ; green
    (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#89dceb")   ; sky
    (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#89b4fa")   ; blue
    (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#cba6f7")   ; mauve
    (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#f5c2e7")   ; pink2
    (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#eba0ac")   ; red
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#f38ba8" :weight 'bold)
    (set-face-attribute 'rainbow-delimiters-mismatched-face nil :foreground "#f38ba8" :weight 'bold)))

;; ====== Git 工具 ======
(use-package magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :demand t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq diff-hl-draw-borders nil))

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

;; ====== 补全框架 ======
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :config
  (setq vertico-count 10))

(use-package marginalia
  :ensure t
  :demand t
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-c r" . consult-recent-file)
         ("C-c s f" . consult-find)
         ("C-c s g" . consult-grep)
         ("C-c s l" . consult-line)
         ("C-c s m" . consult-mark)
         ("C-c s i" . consult-imenu)))

;; ====== 快速导航 ======
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :config
  (setq avy-background t)
  (setq avy-style 'at-full)
  (setq avy-timeout-seconds 0.5)
  (setq avy-all-windows t)
  
  ;; Catppuccin 配色
  (with-eval-after-load 'avy
    (set-face-attribute 'avy-lead-face nil
                        :foreground "#1e1e2e"
                        :background "#f38ba8"
                        :weight 'bold)
    (set-face-attribute 'avy-lead-face-0 nil
                        :foreground "#1e1e2e"
                        :background "#89b4fa"
                        :weight 'bold)
    (set-face-attribute 'avy-lead-face-1 nil
                        :foreground "#1e1e2e"
                        :background "#94e2d5"
                        :weight 'bold)
    (set-face-attribute 'avy-lead-face-2 nil
                        :foreground "#1e1e2e"
                        :background "#f9e2af"
                        :weight 'bold)))

;; ====== 多光标编辑 ======
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :config
  (setq mc/always-run-for-all t)
  
  ;; Catppuccin 配色
  (with-eval-after-load 'multiple-cursors
    (set-face-attribute 'mc/cursor-face nil
                        :foreground "#1e1e2e"
                        :background "#f38ba8")
    (set-face-attribute 'mc/cursor-bar-face nil
                        :background "#f38ba8"
                        :height 1)))

;; ====== 代码补全 ======
(use-package company
  :ensure t
  :demand t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete-selection)
              ([tab] . company-complete-selection)
              ("<return>" . company-complete-selection)
              ("RET" . company-complete-selection))
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-require-match nil)
  (setq company-show-quick-access t)
  (setq company-tooltip-limit 10)
  (setq company-tooltip-minimum-width 40)
  (setq company-tooltip-maximum-width 80)
  (setq company-tooltip-margin 2)
  (global-company-mode 1))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 10)
  (setq company-box-doc-delay 0.3)
  (setq company-box-doc-enable t)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (setq company-box-backends-colors nil)
  (setq company-box-scrollbar t)
  
  ;; 自定义补全框样式
  (setq company-box-frame-parameters
        '((internal-border-width . 1)
          (left-fringe . 8)
          (right-fringe . 8)))
  
  ;; 自定义 company-box faces
  (with-eval-after-load 'company-box
    (set-face-attribute 'company-box-background nil
                        :background nil
                        :inherit 'default)
    (set-face-attribute 'company-box-selection nil
                        :background "#45475a"
                        :foreground "#cdd6f4"
                        :weight 'bold)
    (set-face-attribute 'company-box-scrollbar nil
                        :background "#6c7086")
    (set-face-attribute 'company-box-candidate nil
                        :foreground "#cdd6f4")))

;; ====== LSP 支持 ======
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-enable-snippet t
        lsp-idle-delay 0.2
        lsp-enable-file-watchers nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-diagnostics-enable t
        lsp-completion-provider :capf
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-log-io nil
        lsp-signature-auto-activate t
        lsp-signature-render-documentation nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-enable-symbol-highlighting nil
        lsp-lens-enable nil
        lsp-semantic-tokens-enable nil))  ;; 禁用语义高亮以提升性能

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-delay 0.1
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t))

(use-package lsp-java
  :ensure t
  :demand t
  :config
  ;; 禁用不必要的功能以提升性能
  (setq lsp-java-references-code-lens-enabled nil
        lsp-java-implementations-code-lens-enabled nil
        lsp-java-progress-reports-enabled nil))

;; ====== 开发工具 ======
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'left-fringe
        flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; ====== HTML/JSX 支持 ======
(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t)
  (setq emmet-preview-default t)
  (setq emmet-move-cursor-between-quotes t))

;; ====== 项目管理 ======
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :config
  (setq projectile-completion-system 'consult))

(provide 'init-packages)
;;; init-packages.el ends here