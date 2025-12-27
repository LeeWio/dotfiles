;;; tsx-mode-config.el --- TSX mode specific configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Specific configuration for TSX mode to handle JSX properly

;;; Code:

;; Ensure proper JSX handling in TSX files
(defun my/setup-tsx-jsx-support ()
  "Setup JSX support for TSX files."
  (when (or (eq major-mode 'tsx-ts-mode)
            (string-match "\\.tsx\\'" (buffer-file-name)))
    ;; Set up proper JSX handling
    (setq-local lsp-idle-delay 0.5)
    ;; Add any necessary JSX-specific configurations here
    (setq-local typescript-indent-level 2)))

;; Add to tsx-ts-mode hook
(add-hook 'tsx-ts-mode-hook 'my/setup-tsx-jsx-support)

(provide 'tsx-mode-config)
;;; tsx-mode-config.el ends here