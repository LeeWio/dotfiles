;;; tsx-mode-config.el --- TSX mode specific configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Specific configuration for TSX mode to handle JSX properly

;;; Code:

;; Ensure proper JSX handling in TSX files
(defun my/setup-tsx-jsx-support ()
  "Setup JSX support for TSX files."
  (when (or (eq major-mode 'typescript-mode)
            (string-match "\\.tsx\\'" (buffer-file-name)))
    ;; Set up proper JSX handling
    (setq-local eglot-send-changes-idle-time 0.5)
    ;; Add any necessary JSX-specific configurations here
    ))

;; Add to typescript-mode hook
(add-hook 'typescript-mode-hook 'my/setup-tsx-jsx-support)

(provide 'tsx-mode-config)
;;; tsx-mode-config.el ends here