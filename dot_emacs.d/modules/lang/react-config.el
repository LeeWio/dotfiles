;;; react-config.el --- React configuration -*- lexical-binding: t -*-

;;; Commentary:
;; React/JSX specific configuration for frontend development

;;; Code:

;; JSX configuration
(require 'jsx-config)

;; Emmet mode for web development (HTML/CSS/JSX expansion)
(use-package emmet-mode
  :ensure t
  :hook (html-mode css-mode js-mode js-ts-mode typescript-mode tsx-ts-mode web-mode sgml-mode)
  :config
  (defun my-emmet-mode-setup ()
    "Setup for emmet-mode."
    (setq emmet-move-cursor-between-quotes t))
  (add-hook 'emmet-mode-hook 'my-emmet-mode-setup))

;; Additional React-specific configurations
(defun my/react-mode-setup ()
  "Setup for React development."
  ;; Set indentation for React files
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(add-hook 'js-ts-mode-hook 'my/react-mode-setup)
(add-hook 'tsx-ts-mode-hook 'my/react-mode-setup)

(provide 'react-config)
;;; react-config.el ends here