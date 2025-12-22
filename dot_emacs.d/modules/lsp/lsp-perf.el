;;; lsp-perf.el --- Performance optimizations for LSP and Corfu -*- lexical-binding: t -*-

;;; Commentary:
;; Performance optimizations for LSP and Corfu systems

;;; Code:

;; Performance tuning for LSP
(defcustom my/lsp-performance-settings
  '((gc-cons-threshold . 100000000)        ;; 100MB during LSP processing
    (read-process-output-max . 1048576)    ;; 1MB for process output
    (lsp-completion-provider . :capf)      ;; Use Corfu for completion
    (lsp-idle-delay . 0.5)                 ;; Delay before lsp triggers
    (lsp-enable-links . nil)               ;; Disable links for performance
    (lsp-enable-folding . nil)             ;; Disable folding for performance
    (lsp-log-io . nil)                     ;; Disable logging for performance
    (lsp-restart . 'ignore)                ;; Don't restart automatically
    (lsp-headerline-breadcrumb-enable . nil) ;; Disable breadcrumb for performance
    (lsp-modeline-code-actions-enable . nil) ;; Disable modeline code actions
    (lsp-modeline-diagnostics-enable . nil)  ;; Disable modeline diagnostics
    (lsp-signature-auto-activate . nil))   ;; Disable signature help
  "Performance settings for LSP.")

;; Performance tuning for Corfu
(defcustom my/corfu-performance-settings
  '((corfu-auto-delay . 0.1)               ;; Short delay for auto completion
    (corfu-popupinfo-delay . 0.2)          ;; Delay for popup info
    (corfu-count . 10)                     ;; Limit number of candidates
    (corfu-max-width . 60)                 ;; Limit width of candidates
    (corfu-scroll-margin . 3))             ;; Reduce scroll margin
  "Performance settings for Corfu.")

;; Apply performance settings when entering prog-mode
(defun my/apply-lsp-performance-settings ()
  "Apply performance settings when LSP is active."
  (dolist (setting my/lsp-performance-settings)
    (set (car setting) (cdr setting))))

(defun my/reset-lsp-performance-settings ()
  "Reset performance settings when LSP is inactive."
  (setq gc-cons-threshold (* 100 1024 1024))  ;; Back to 100MB
  (setq read-process-output-max (* 1024 1024))) ;; Back to 1MB

;; Optimize file watching for large projects
(setq lsp-file-watch-threshold 10000)  ;; Increase file watch threshold

;; Optimize completion performance
(setq completion-cycle-threshold 3)            ;; Cycle after 3 completions
(setq tab-always-indent 'complete)             ;; Tab for completion

;; Performance hooks
(add-hook 'lsp-mode-hook 'my/apply-lsp-performance-settings)
(add-hook 'lsp-mode-disable-hook 'my/reset-lsp-performance-settings)

;; Throttle expensive operations
(defun my/throttle-expensive-operations ()
  "Throttle expensive operations in large buffers."
  (when (> (buffer-size) (* 1000 1000))  ;; 1MB
    ;; Reduce syntax highlighting
    (font-lock-mode -1)
    ;; Disable line numbers in large files
    (display-line-numbers-mode -1)
    ;; Reduce fringe indicators
    (setq indicate-empty-lines nil)
    (setq indicate-buffer-boundaries nil)))

(add-hook 'find-file-hook 'my/throttle-expensive-operations)

(provide 'lsp-perf)
;;; lsp-perf.el ends here