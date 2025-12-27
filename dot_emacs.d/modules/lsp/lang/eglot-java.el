;;; eglot-java.el --- Eglot config for Java -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot setup for Java development using jdtls

;;; Code:

(use-package eglot
  :ensure t
  :hook
  ((java-mode) . eglot-ensure)   ;; 自动启用 Eglot
  :config
  ;; -----------------------------
  ;; LSP server setup
  ;; -----------------------------
  (if (executable-find "jdtls")
      (progn
        (message "[Eglot] Using jdtls for Java")
        (add-to-list 'eglot-server-programs
                     '(java-mode . ("jdtls"))))
    (message "[Eglot] jdtls NOT FOUND! Please install jdtls."))

  ;; -----------------------------
  ;; QoL tweaks
  ;; -----------------------------
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; inlay hints (Emacs29+)
              (when (fboundp 'eglot-inlay-hints-mode)
                (eglot-inlay-hints-mode 1))
              ;; 自动关闭
              (setq-local eglot-autoshutdown t)
              ;; 避免阻塞
              (setq-local eglot-sync-connect nil)
              ;; xref 支持
              (setq-local eglot-extend-to-xref t)
              ;; 保存时自动格式化（可选）
              ;; (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
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

(provide 'eglot-java)
;;; eglot-java.el ends here
