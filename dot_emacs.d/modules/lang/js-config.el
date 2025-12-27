;;; js-config.el --- JavaScript configuration -*- lexical-binding: t -*-

;;; Commentary:
;; JavaScript configuration for frontend development

;;; Code:

;; JavaScript mode configuration
(use-package js
  :mode ("\\.js\\'" . js-mode)
  :mode ("\\.jsx\\'" . js-mode)
  :config
  (defun my/js-mode-setup ()
    "Setup for js-mode."
    (setq js-indent-level 2)
    ;; Enable JSX syntax
    (setq js-jsx-browser-refresh-delay 0.5))
  (add-hook 'js-mode-hook 'my/js-mode-setup))

;; JSON mode configuration
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :mode ("\\.jsonc\\'" . json-mode)
  :config
  (defun my-json-mode-setup ()
    "Setup for json-mode."
    (setq js-indent-level 2))
  (add-hook 'json-mode-hook 'my-json-mode-setup))

(provide 'js-config)
;;; js-config.el ends here