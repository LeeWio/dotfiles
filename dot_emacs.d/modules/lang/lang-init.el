;;; lang-init.el --- Language support configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Programming language support with performance considerations

;;; Code:

;; Web development
(defun my/web-mode-setup ()
  "Setup for web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-pairing nil))

(add-hook 'web-mode-hook 'my/web-mode-setup)

;; JavaScript
(defun my/js-mode-setup ()
  "Setup for js-mode."
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'my/js-mode-setup)

;; Python
(defun my/python-mode-setup ()
  "Setup for python-mode."
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4))

(add-hook 'python-mode-hook 'my/python-mode-setup)

;; Go
(defun my/go-mode-setup ()
  "Setup for go-mode."
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save nil t))

(add-hook 'go-mode-hook 'my/go-mode-setup)

;; Rust
(defun my/rust-mode-setup ()
  "Setup for rust-mode."
  (setq rust-format-on-save t))

(add-hook 'rust-mode-hook 'my/rust-mode-setup)

;; Disable syntax highlighting in large files for performance
(defun my/disable-syntax-highlighting-if-large ()
  "Disable syntax highlighting in large buffers."
  (when (> (buffer-size) (* 1000 1000)) ; 1MB
    (syntax-table (copy-syntax-table))
    (modify-syntax-entry ?_ "w")
    (jit-lock-mode -1)))

(add-hook 'find-file-hook 'my/disable-syntax-highlighting-if-large)

(provide 'lang-init)
;;; lang-init.el ends here