;;; local-settings.el --- 本机特定设置
;; 此文件不会加入版本控制

;; 示例：设置代理（如果需要）
;; (setq url-proxy-services
;;       '(("http" . "localhost:7890")
;;         ("https" . "localhost:7890")))

;; 示例：个性化设置
(setq user-full-name "你的名字")
(setq user-mail-address "your.email@example.com")

;; 示例：根据主机名设置
(cond
 ((string-match-p "work" (system-name))
  (setq default-directory "~/work/"))
 ((string-match-p "home" (system-name))
  (setq default-directory "~/documents/")))

(provide 'local-settings)
;;; local-settings.el ends here
