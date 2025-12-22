;;; eglot-c.el --- Eglot configuration for C/C++ -*- lexical-binding: t -*-

;;; Commentary:
;; Simple eglot configuration for C/C++ development

;;; Code:

;; Eglot configuration for C/C++
(with-eval-after-load 'eglot
  ;; Register clangd as the LSP server for C/C++
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd" 
                                     "--background-index"
                                     "--clang-tidy"))))

;; Simple key bindings
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c g") 'eglot-find-definition)
  (define-key eglot-mode-map (kbd "C-c b") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c p") 'eglot-find-references)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions))

(provide 'eglot-c)
;;; eglot-c.el ends here