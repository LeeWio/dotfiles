;;; ts-config.el --- TypeScript configuration -*- lexical-binding: t -*-

;;; Commentary:
;; TypeScript configuration for frontend development

;;; Code:

;; TypeScript configuration for tree-sitter
(when (treesit-available-p)
  (defun my/typescript-mode-setup ()
    "Setup for typescript tree-sitter modes."
    (setq typescript-indent-level 2)
    ;; Enable JSX syntax in typescript modes
    (setq js-jsx-browser-refresh-delay 0.5))
  (add-hook 'tsx-ts-mode-hook 'my/typescript-mode-setup)
  (add-hook 'typescript-ts-mode-hook 'my/typescript-mode-setup))

(provide 'ts-config)
;;; ts-config.el ends here