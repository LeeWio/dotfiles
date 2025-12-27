;;; lsp-perf.el --- Performance optimizations for Eglot & Corfu -*- lexical-binding: t -*-

;;; Commentary:
;; Practical performance enhancements tailored for:
;;  - Eglot (Emacs 29+)
;;  - Corfu based completion workflow
;;
;; Safe, modern, and minimal.

;;; Code:

;; =========================
;; GC + IO Performance
;; =========================
(setq gc-cons-threshold (* 200 1024 1024))  ;; 200MB
(setq read-process-output-max (* 4 1024 1024)) ;; 4MB improves LSP throughput

;; =========================
;; Eglot Performance
;; =========================
(with-eval-after-load 'eglot
  ;; Do NOT spam minibuffer
  (setq eglot-events-buffer-size 0)

  ;; Disable too-aggressive diagnostics refresh
  (setq eglot-report-progress nil)

  ;; Don't watch insane number of files
  (setq eglot-sync-connect nil) ;; async startup = faster UI
)

;; =========================
;; Completion Performance
;; =========================
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Corfu performance tuning
(with-eval-after-load 'corfu
  (setq corfu-auto-delay 0.08)
  (setq corfu-auto-prefix 1)
  (setq corfu-popupinfo-delay '(0.4 . 0.1)))

;; =========================
;; Large File Optimizer
;; =========================
(defun my/large-file-optimizations ()
  "Apply optimizations for large files."
  (when (> (buffer-size) (* 2 1024 1024)) ;; > 2MB
    (setq-local gc-cons-threshold (* 400 1024 1024))
    (setq-local bidi-display-reordering nil)
    (setq-local bidi-paragraph-direction 'left-to-right)
    (when (fboundp 'so-long-mode)
      (so-long-mode 1))
    (when (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode -1))))

(add-hook 'find-file-hook #'my/large-file-optimizations)

;; =========================
;; UI Noise Reduction
;; =========================
(setq mode-line-compact t)

;; =========================
;; TRAMP (remote file) performance
;; =========================
(setq remote-file-name-inhibit-locks t)

(provide 'lsp-perf)
;;; lsp-perf.el ends here