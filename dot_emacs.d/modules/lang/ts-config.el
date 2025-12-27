;;; ts-config.el --- TypeScript configuration -*- lexical-binding: t -*-

;;; Commentary:
;; TypeScript configuration for frontend development

;;; Code:

;; TypeScript mode configuration
(use-package typescript-mode
  :ensure t
  :config
  (defun my/typescript-mode-setup ()
    "Setup for typescript-mode."
    (setq typescript-indent-level 2)
    ;; Enable JSX syntax in typescript-mode
    (setq js-jsx-browser-refresh-delay 0.5))
  (add-hook 'typescript-mode-hook 'my/typescript-mode-setup))

(provide 'ts-config)
;;; ts-config.el ends here