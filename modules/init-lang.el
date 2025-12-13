;;; init-lang.el --- 通用语言配置

;; 加载各语言专用配置
(load (expand-file-name "modules/init-java.el" user-emacs-directory))
(load (expand-file-name "modules/init-typescript.el" user-emacs-directory))

;; ====== Web 开发 ======
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (setq js-indent-level 2))

(use-package sgml-mode
  :ensure t
  :mode "\\.html\\'")

;; ====== Tailwind CSS ======
(use-package lsp-tailwindcss
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t))

;; ====== Emmet (HTML/CSS 快速编写) ======
(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)
         (typescript-mode . emmet-mode)
         (rjsx-mode . emmet-mode))
  :config
  (setq emmet-expand-jsx-className? t))

;; ====== Prettier 格式化 ======
(use-package prettier-js
  :ensure t
  :hook ((typescript-mode . prettier-js-mode)
         (js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)))

(provide 'init-lang)
;;; init-lang.el ends here