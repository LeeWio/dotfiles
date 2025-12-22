;;; perf-init.el --- Performance tuning -*- lexical-binding: t -*-

;;; Commentary:
;; Performance tuning configurations

;;; Code:

;; Garbage collection optimizations
(defun my/minimize-gc ()
  "Minimize garbage collection during startup."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/reset-gc ()
  "Reset garbage collection threshold after startup."
  (setq gc-cons-threshold (* 100 1024 1024)))

(my/minimize-gc)
(add-hook 'after-init-hook 'my/reset-gc)

;; Startup optimizations
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; File name handling
(setq vc-handled-backends nil)
(setq find-file-suppress-same-file-warnings t)

;; Reduce rendering
(setq auto-window-vscroll nil)
(setq hscroll-margin 2)
(setq hscroll-step 1)

;; Process performance
(setq w32-pipe-read-delay 0)
(setq w32-pipe-buffer-size (* 64 1024))

;; Idle timers
(setq idle-update-delay 1.0)

;; File watchers
(setq auto-revert-check-vc-info nil)
(setq auto-revert-use-notify nil)

;; Benchmarking
(defun my/display-startup-time ()
  "Display startup time."
  (message "Emacs loaded in %.2f seconds with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(add-hook 'after-init-hook 'my/display-startup-time)

(provide 'perf-init)
;;; perf-init.el ends here