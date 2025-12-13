;;; compile-config.el --- 编译配置文件以提高性能

(defun my/compile-config ()
  "编译所有Emacs Lisp配置文件。"
  (interactive)
  (byte-recompile-directory user-emacs-directory 0 t)
  (message "所有配置文件已编译！重启Emacs获取最佳性能。"))

(global-set-key (kbd "C-c c c") 'my/compile-config)

(provide 'compile-config)
;;; compile-config.el ends here
