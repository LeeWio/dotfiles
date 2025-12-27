;;; file-associations.el --- File type associations -*- lexical-binding: t -*-

;;; Commentary:
;; File type associations for different programming languages

;;; Code:

;; File associations
(add-to-list 'auto-mode-alist '("\\.log\\'" . text-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . text-mode))
;; Tree-sitter associations are handled separately in the treesit configuration
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(provide 'file-associations)
;;; file-associations.el ends here