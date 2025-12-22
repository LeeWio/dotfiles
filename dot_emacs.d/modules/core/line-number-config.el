;;; line-number-config.el --- Line number configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for line numbers and current line highlighting

;;; Code:

;; Line number configuration
(setq display-line-numbers-type 'relative) ; Relative line numbers by default

;; Enable line numbers globally
(global-display-line-numbers-mode 1)

;; Customize line number appearance
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-width-start t)

;; Mode-specific line number settings
(defun my/setup-text-mode-line-numbers ()
  "Setup line numbers for text modes."
  (setq display-line-numbers-type 'relative))

(defun my/setup-programming-mode-line-numbers ()
  "Setup line numbers for programming modes."
  (setq display-line-numbers-type 'relative))

(defun my/setup-org-mode-line-numbers ()
  "Setup line numbers for org mode."
  (setq display-line-numbers-type 'relative))

;; Hook configurations
(add-hook 'text-mode-hook 'my/setup-text-mode-line-numbers)
(add-hook 'prog-mode-hook 'my/setup-programming-mode-line-numbers)
(add-hook 'org-mode-hook 'my/setup-org-mode-line-numbers)

;; Disable line numbers in certain modes
(dolist (hook '(term-mode-hook eshell-mode-hook shell-mode-hook 
              vterm-mode-hook pdf-view-mode-hook
              dashboard-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(provide 'line-number-config)
;;; line-number-config.el ends here