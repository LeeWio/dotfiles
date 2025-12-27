;;; clipboard-config.el --- Clipboard configuration for Wayland -*- lexical-binding: t -*-

;;; Commentary:
;; Clipboard integration for Wayland systems using wl-clipboard

;;; Code:

;; Check if we're on Wayland and not in a graphical environment
(when (and (not (display-graphic-p))
           (string-equal (getenv "XDG_SESSION_TYPE") "wayland")
           (executable-find "wl-copy")
           (executable-find "wl-paste"))
  ;; Copy to system clipboard using wl-copy
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let ((process-connection-type nil))
            (let ((proc (start-process "wl-copy" nil "wl-copy" "-f")))
              (process-send-string proc text)
              (process-send-eof proc)))))

  ;; Paste from system clipboard using wl-paste
  (setq interprogram-paste-function
        (lambda ()
          (let ((output (shell-command-to-string "wl-paste -n")))
            (unless (string= output "")
              (substring-no-properties output))))))

(provide 'clipboard-config)
;;; clipboard-config.el ends here