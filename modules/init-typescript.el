;;; init-typescript.el --- TypeScript/JavaScript 开发配置

;; TypeScript 配置
(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . rjsx-mode))
  :config
  (setq typescript-indent-level 2))

;; JavaScript 配置
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  (setq js-indent-level 2))

;; JSX 配置
(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'"
  :config
  (setq js2-basic-offset 2))

;; 添加 LSP hooks
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'lsp)

(provide 'init-typescript)
;;; init-typescript.el ends here