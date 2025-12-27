;;; eglot-ts-config.el --- Eglot configuration for TypeScript -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot configuration specifically for TypeScript / JavaScript

;;; Code:

(use-package eglot
  :ensure t
  :hook
  ((typescript-mode js-mode tsx-ts-mode) . eglot-ensure)

  :config
  ;; -----------------------------
  ;; Detect language servers
  ;; -----------------------------
  (let ((ts-server-found (executable-find "typescript-language-server"))
        (vtsls-found (executable-find "vtsls"))  ; Volta TypeScript Language Server for better Next.js support
        )
    (cond
     ;; Use vtsls if available (better for Next.js/React projects)
     (vtsls-found
      (message "[Eglot] Using vtsls (Volta TypeScript Language Server)")
      (add-to-list 'eglot-server-programs
                   '((typescript-mode js-mode)
                     .
                     ("vtsls" "--stdio")))
      ;; Add tree-sitter support if available
      (when (treesit-available-p)
        (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
        (add-hook 'js-ts-mode-hook #'eglot-ensure)
        (add-to-list 'eglot-server-programs
                     '((tsx-ts-mode js-ts-mode)
                       .
                       ("vtsls" "--stdio")))))
     
     ;; Fallback to typescript-language-server
     (ts-server-found
      (message "[Eglot] Using typescript-language-server")
      (add-to-list 'eglot-server-programs
                   '((typescript-mode js-mode)
                     .
                     ("typescript-language-server" "--stdio")))
      ;; Add tree-sitter support if available
      (when (treesit-available-p)
        (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
        (add-hook 'js-ts-mode-hook #'eglot-ensure)
        (add-to-list 'eglot-server-programs
                     '((tsx-ts-mode js-ts-mode)
                       .
                       ("typescript-language-server" "--stdio")))))
     
     ;; No server found
     (t
      (message "[Eglot] No TypeScript language server found! Install via: npm i -g typescript-language-server typescript OR npm i -g @volar/vscode-typescript-languageserver"))))

  ;; -----------------------------
  ;; QoL tweaks
  ;; -----------------------------
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Inlay hints (Emacs29+)
              (when (fboundp 'eglot-inlay-hints-mode)
                (eglot-inlay-hints-mode 1))

              ;; 避免阻塞
              (setq-local eglot-sync-connect nil)

              ;; 自动重启
              (setq-local eglot-autoshutdown t)

              ;; xref 支持
              (setq-local eglot-extend-to-xref t)

              ;; Format on save for frontend development
              (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
              
                            )))

;; -----------------------------
;; Key bindings
;; -----------------------------
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c d") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c g d") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c g r") 'xref-find-references)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format-buffer))

(provide 'eglot-ts-config)
;;; eglot-ts-config.el ends here