;;; jsx-config.el --- JSX configuration -*- lexical-binding: t -*-

;;; Commentary:
;; JSX-specific configuration for React development

;;; Code:

;; Add support for JSX/TSX files with tree-sitter if available, otherwise fallback to basic modes
(when (treesit-available-p)
  ;; Check if tree-sitter grammars are available, and use them if they are
  (when (treesit-language-available-p 'javascript)
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode)))
  (when (treesit-language-available-p 'tsx)
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))))

(provide 'jsx-config)
;;; jsx-config.el ends here