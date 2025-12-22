;;; custom.el --- Custom configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Personal customizations

;;; Code:

;; Custom variables
(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))

;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'custom)
;;; custom.el ends here