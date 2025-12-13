;;; init-keybindings.el --- 快捷键配置
;; 配置全局快捷键

;; 常用命令
(global-set-key (kbd "C-x r") 'replace-string)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-r") 'revert-buffer)
(global-set-key (kbd "C-x g") 'magit-status)

;; 窗口操作
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x 1") 'delete-other-windows)
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)
(global-set-key (kbd "C-x 0") 'delete-window)

;; 缓冲区切换
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'switch-to-buffer)

(defun my-eshell ()
  "智能打开 eshell。"
  (interactive)
  (if (get-buffer "*eshell*")
      (switch-to-buffer "*eshell*")
    (eshell)))

(global-set-key (kbd "C-c e") 'my-eshell)

;; 搜索
(global-set-key (kbd "C-c f") 'consult-find)
(global-set-key (kbd "C-c g") 'consult-grep)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
