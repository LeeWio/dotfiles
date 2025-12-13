;;; init.el --- 主配置入口

;; 记录启动时间
(defvar my/init-start-time (current-time)
  "记录Emacs初始化开始时间。")

;; 包管理配置
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; 确保 use-package 已安装
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 配置 use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t
      use-package-compute-statistics nil)  ;; 禁用统计以提升性能

;; 性能优化设置
(setq auto-save-default nil)  ;; 禁用自动保存（使用手动保存）
(setq create-lockfiles nil)  ;; 禁用锁文件
(setq make-backup-files t)   ;; 保留备份文件
(setq vc-follow-symlinks t)  ;; 自动跟随符号链接
(setq large-file-warning-threshold 100000000)  ;; 100MB 大文件警告

;; 添加 modules 到 load-path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; 加载核心模块
(load (expand-file-name "core/init-benchmark.el" user-emacs-directory) t)
(load (expand-file-name "core/init-vars.el" user-emacs-directory) t)

;; 立即加载的模块（启动时必需）
(dolist (module '("modules/init-ui.el"
                  "modules/init-modeline.el"
                  "modules/init-packages.el"
                  "modules/init-java.el"
                  "modules/init-typescript.el"
                  "modules/init-lang.el"
                  "modules/init-markdown.el"
                  "modules/init-org.el"))
  (load (expand-file-name module user-emacs-directory) t))

;; 延迟加载的模块
(defvar my/deferred-modules
  '("modules/init-editing.el"
    "modules/init-keybindings.el"
    "extensions/compile-config.el")
  "延迟加载的模块列表")

;; 启动后初始化
(defun my/post-init-setup ()
  "启动后优化设置。"
  ;; 恢复正常的GC阈值（启动完成后）
  (setq gc-cons-threshold (* 16 1024 1024))  ;; 16MB
  (setq gc-cons-percentage 0.1)
  
  ;; 运行一次GC清理
  (run-with-idle-timer 2 nil #'garbage-collect)
  
  (message "Emacs启动完成，耗时: %.2f秒" (my/elapsed-time))
  
  ;; 延迟加载非关键模块
  (dolist (module my/deferred-modules)
    (run-with-idle-timer
     0.5 nil
     (lambda (f)
       (load f t))
     (expand-file-name module user-emacs-directory))))

(add-hook 'emacs-startup-hook #'my/post-init-setup)

;; 加载本地配置（如果存在）
(when (file-exists-p (expand-file-name "config/local-settings.el" user-emacs-directory))
  (load (expand-file-name "config/local-settings.el" user-emacs-directory) t))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background unspecified :box nil))))
 '(mode-line-inactive ((t (:background unspecified :box nil)))))
