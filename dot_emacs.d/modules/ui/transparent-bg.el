;;; transparent-bg.el --- Transparent background configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for transparent background in GUI mode

;;; Code:

;; Frame parameter for transparency (only in GUI mode)
(when (display-graphic-p)
  ;; Set frame background to transparent
  (set-frame-parameter nil 'alpha-background 0)
  
  ;; For new frames
  (add-to-list 'default-frame-alist '(alpha-background . 0))
  
  ;; Function to apply transparency to all frames
  (defun my/set-all-frames-transparent ()
    "Set all existing frames to have transparent background."
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'alpha-background 0)))
  
  ;; Apply transparency after init
  (add-hook 'after-init-hook 'my/set-all-frames-transparent)
  
  ;; Apply transparency to new frames
  (defun my/apply-transparency-to-new-frame (frame)
    "Apply transparency to newly created FRAME."
    (set-frame-parameter frame 'alpha-background 0))
  
  (add-hook 'after-make-frame-functions 'my/apply-transparency-to-new-frame))

(provide 'transparent-bg)
;;; transparent-bg.el ends here