;;; web-config.el --- Web development configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Basic web development configuration

;;; Code:

;; Web mode configuration (basic)
(defun my/web-mode-setup ()
  "Setup for web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-pairing nil))

(add-hook 'web-mode-hook 'my/web-mode-setup)

(provide 'web-config)
;;; web-config.el ends here