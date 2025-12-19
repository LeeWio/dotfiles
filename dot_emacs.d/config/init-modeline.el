;;; init-modeline.el --- Emacs DOOM Modeline 配置

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: modeline

;;; Commentary:

;; DOOM Modeline 配置文件
;; 使用专业的 DOOM Modeline 替代默认状态栏

;;; Code:

;; DOOM Modeline 配置
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  ;; If non-nil, cause imenu to see `doom-modeline' declarations.
  (setq doom-modeline-support-imenu t)

  ;; How tall the mode-line should be. It's only respected in GUI.
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be. It's only respected in GUI.
  (setq doom-modeline-bar-width 3)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (setq doom-modeline-hud nil)

  ;; The limit of the window width.
  (setq doom-modeline-window-width-limit 85)

  ;; How to detect the project root.
  (setq doom-modeline-project-detection 'auto)

  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Whether display icons in the mode-line.
  (setq doom-modeline-icon t)

  ;; Whether display the icon for `major-mode'.
  (setq doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  (setq doom-modeline-major-mode-color-icon t)

  ;; Whether display the icon for the buffer state.
  (setq doom-modeline-buffer-state-icon t)

  ;; Whether display the modification icon for the buffer.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether display the lsp icon.
  (setq doom-modeline-lsp-icon t)

  ;; Whether display the time icon.
  (setq doom-modeline-time-icon t)

  ;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
  (setq doom-modeline-unicode-fallback nil)

  ;; Whether display the buffer name.
  (setq doom-modeline-buffer-name t)

  ;; Whether highlight the modified buffer name.
  (setq doom-modeline-highlight-modified-buffer-name t)

  ;; When non-nil, mode line displays column numbers zero-based.
  (setq doom-modeline-column-zero-based t)

  ;; Specification of "percentage offset" of window through buffer.
  (setq doom-modeline-percent-position '(-3 "%p"))

  ;; Format used to display line numbers in the mode line.
  (setq doom-modeline-position-line-format '("L%l"))

  ;; Format used to display column numbers in the mode line.
  (setq doom-modeline-position-column-format '("C%c"))

  ;; Whether display the minor modes in the mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; Whether display the selection information.
  (setq doom-modeline-selection-info t)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count nil)

  ;; Whether display the buffer position information.
  (setq doom-modeline-enable-buffer-position t)

  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding nil)

  ;; Whether display the indentation information.
  (setq doom-modeline-indent-info nil)

  ;; Whether display the total line number.
  (setq doom-modeline-total-line-number nil)

  ;; Whether display the icon of vcs segment. It respects option `doimo-modeline-icon'.
  (setq doom-modeline-vcs-icon t)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 20)

  ;; Whether display the icon of check segment. It respects option `doom-modeline-icon'.
  (setq doom-modeline-check-icon t)

  ;; If non-nil, only display one number for check information if applicable.
  (setq doom-modeline-check-simple-format t)

  ;; Whether display the project name. Non-nil to display in the mode-line.
  (setq doom-modeline-project-name t)

  ;; Whether display the `lsp' state. Non-nil to display in the mode-line.
  (setq doom-modeline-lsp t)

  ;; Whether display the GitHub notifications. It requires `ghub' package.
  (setq doom-modeline-github nil)

  ;; Whether display the modal state.
  (setq doom-modeline-modal nil)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (setq doom-modeline-mu4e nil)

  ;; Whether display the gnus notifications.
  (setq doom-modeline-gnus nil)

  ;; Whether display the IRC notifications. It requires `circe' or `erc' package.
  (setq doom-modeline-irc nil)

  ;; Whether display the battery status. It respects `display-battery-mode'.
  (setq doom-modeline-battery nil)

  ;; Whether display the time. It respects `display-time-mode'.
  (setq doom-modeline-time nil)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version nil))

;; Modeline 透明背景以适配 Catppuccin 主题
(with-eval-after-load 'doom-modeline
  (custom-set-faces
   '(mode-line ((t (:background nil :box nil))))
   '(mode-line-inactive ((t (:background nil :box nil))))))

(provide 'init-modeline)

;;; init-modeline.el ends here