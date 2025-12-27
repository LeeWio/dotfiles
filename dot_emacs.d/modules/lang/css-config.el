;;; css-config.el --- CSS and Tailwind configuration -*- lexical-binding: t -*-

;;; Commentary:
;; CSS, SCSS and Tailwind CSS configuration for frontend development

;;; Code:

;; CSS/SCSS support
(use-package css-mode
  :mode ("\\.css\\'" . css-mode)
  :mode ("\\.scss\\'" . scss-mode)
  :config
  (defun my-css-mode-setup ()
    "Setup for css-mode."
    (setq css-indent-offset 2))
  (add-hook 'css-mode-hook 'my-css-mode-setup))

;; Tailwind CSS support (using css-mode with additional functionality)
(use-package css-mode
  :mode ("\\.tailwind\\.config\\.js\\'" . js-mode)
  :mode ("\\.tailwind\\.config\\.cjs\\'" . js-mode)
  :mode ("tailwind\\.config\\.js\\'" . js-mode)
  :mode ("tailwind\\.config\\.cjs\\'" . js-mode))

(provide 'css-config)
;;; css-config.el ends here