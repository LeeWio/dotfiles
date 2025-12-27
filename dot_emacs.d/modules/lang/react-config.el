;;; react-config.el --- React configuration -*- lexical-binding: t -*-

;;; Commentary:
;; React/JSX specific configuration for frontend development

;;; Code:

;; Web mode for JSX/Vue files
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :mode ("\\.vue\\'" . web-mode)
  :config
  (defun my-web-mode-setup ()
    "Setup for web-mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-engines-alist
          '(("jsx" . "\\.js[x]?\\'")))
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-auto-quoting t))
  (add-hook 'web-mode-hook 'my-web-mode-setup))

;; JSX configuration
(require 'jsx-config)

;; Emmet mode for web development (HTML/CSS/JSX expansion)
(use-package emmet-mode
  :ensure t
  :hook (html-mode css-mode js-mode typescript-mode web-mode sgml-mode)
  :config
  (defun my-emmet-mode-setup ()
    "Setup for emmet-mode."
    (setq emmet-move-cursor-between-quotes t))
  (add-hook 'emmet-mode-hook 'my-emmet-mode-setup))

(provide 'react-config)
;;; react-config.el ends here