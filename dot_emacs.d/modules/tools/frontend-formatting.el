;;; frontend-formatting.el --- Frontend formatting tools -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for frontend development formatting tools like prettier, eslint

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

(provide 'frontend-formatting)
;;; frontend-formatting.el ends here