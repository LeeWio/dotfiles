;;; mwim-config.el --- MWIM (Move Where I Mean) configuration -*- lexical-binding: t -*-

;;; Commentary:
;; MWIM configuration for intelligent cursor movement
;; Provides smart movement to beginning/end of lines/code

;;; Code:

(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end))
  :config
  ;; Configure mwim behavior
  (setq mwim-expand-whitespace-before-indent t) ; Expand whitespace when moving to beginning
  (setq mwim-expand-whitespace-after-eol t)    ; Expand whitespace when moving to end
  (setq mwim-allow-extend-line t)              ; Allow extending line when moving to end
  (setq mwim-expand-whitespace-before-bol t)   ; Expand whitespace when moving to beginning of line
  
  ;; Define custom behavior for different modes
  (defun my-mwim-setup ()
    "Custom setup for mwim in different modes."
    (setq-local mwim-expand-whitespace-before-indent t)
    (setq-local mwim-expand-whitespace-after-eol t)
    (setq-local mwim-allow-extend-line t))
  
  ;; Apply setup to all programming modes
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook 'my-mwim-setup)))

(provide 'mwim-config)
;;; mwim-config.el ends here