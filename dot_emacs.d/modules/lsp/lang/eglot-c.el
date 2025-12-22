;;; eglot-c.el --- Eglot configuration for C/C++ -*- lexical-binding: t -*-

;;; Commentary:
;; Eglot configuration for C/C++ development with clangd

;;; Code:

;; Ensure eglot is available (built into Emacs 29+)
(unless (featurep 'eglot)
  (require 'eglot))

;; C/C++ eglot configuration
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Configure clangd as the LSP server for C/C++
(with-eval-after-load 'eglot
  ;; Register clangd as the LSP server for C/C++
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd" 
                                     "--background-index"
                                     "--clang-tidy"
                                     "--completion-style=detailed"
                                     "--header-insertion=iwyu"
                                     "--header-insertion-decorators=0"))))

;; C/C++ specific eglot settings
(defun my/c-cpp-eglot-setup ()
  "C/C++ specific eglot setup."
  ;; Configure eglot for C/C++
  (setq-local eglot-events-buffer-size 0)  ; Disable events buffer for performance
  (setq-local eglot-sync-connect nil)      ; Connect asynchronously
  (setq-local eglot-autoshutdown t)        ; Shutdown server when buffer is killed
  
  ;; Enable company-capf for better completion integration
  (setq-local completion-at-point-functions 
              (cons #'eglot-completion-at-point completion-at-point-functions))
  )

;; Apply C/C++ specific settings
(add-hook 'c-mode-hook 'my/c-cpp-eglot-setup)
(add-hook 'c++-mode-hook 'my/c-cpp-eglot-setup)

;; C/C++ specific key bindings
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c g") 'eglot-find-definition)
  (define-key eglot-mode-map (kbd "C-c b") 'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c p") 'eglot-find-references)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc-box-help-at-point)
  (define-key eglot-mode-map (kbd "C-c i") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions))

(provide 'eglot-c)
;;; eglot-c.el ends here