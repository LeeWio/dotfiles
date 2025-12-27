;;; frontend-tools.el --- Frontend development tools configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for frontend development tools like prettier, eslint, etc.

;;; Code:

;; Prettier integration for formatting
(use-package prettier-js
  :ensure t
  :hook ((js-mode typescript-mode css-mode scss-mode json-mode web-mode) . prettier-js-mode)
  :config
  (setq prettier-js-show-errors nil))

;; ESLint integration
(use-package flycheck
  :ensure t
  :hook ((js-mode typescript-mode) . flycheck-mode)
  :config
  (setq flycheck-checker 'javascript-eslint
        flycheck-javascript-eslint-executable "eslint"))

;; Configure project management for frontend projects
(use-package project
  :config
  ;; Recognize common frontend project root markers
  (add-to-list 'project-find-functions 'project-try-vcs))

(provide 'frontend-tools)
;;; frontend-tools.el ends here