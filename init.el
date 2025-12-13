;;; init.el --- 主配置入口
;; 第一阶段：设置基本环境

;; 记录开始时间用于性能分析
(defvar my/init-start-time (current-time)
  "记录Emacs初始化开始时间。")

;; 设置包管理目录
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; 初始化包管理系统
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; 确保 use-package 已安装
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 配置 use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-defer t)        ;; 默认延迟加载
(setq use-package-expand-minimally t)    ;; 减少宏扩展
(setq use-package-enable-imenu-support t)

;; 第二阶段：加载核心模块
;; 加载性能分析工具
(load (expand-file-name "core/init-benchmark.el" user-emacs-directory) t)

;; 加载基本变量
(load (expand-file-name "core/init-vars.el" user-emacs-directory) t)

 ;; 第三阶段：设置延迟加载                                                                                                                                                      
 (defvar my/modules-to-load
   '(("modules/init-ui.el" . t)           ;; 启动时加载                                                                                                                         
     ("modules/init-editing.el" . nil)    ;; 延迟加载                                                                                                                           
     ("modules/init-keybindings.el" . nil)
     ("modules/init-packages.el" . nil)
     ("extensions/compile-config.el" . nil))
   "模块列表及其加载时机。t表示启动时加载，nil表示延迟加载")

;; 延迟加载非关键模块
(defun my/load-modules ()
  "加载所有配置模块。"
  (dolist (module my/modules-to-load)
    (let ((file (car module))
          (eager (cdr module)))
      (if eager
          (progn
            (message "立即加载: %s" file)
            (load (expand-file-name file user-emacs-directory) t))
        (run-with-idle-timer
         0.1 nil
         (lambda (f)
           (load f t))
         (expand-file-name file user-emacs-directory))))))

;; 第四阶段：启动后清理
(defun my/post-init-setup ()
  "启动后优化设置。"
  ;; 恢复正常的GC阈值
  (setq gc-cons-threshold (* 2 1000 1000))
  ;; 运行垃圾回收
  (garbage-collect)
  ;; 显示启动时间
  (message "Emacs启动完成，耗时: %.2f秒" (my/elapsed-time))
  ;; 加载模块
  (my/load-modules))

;; 在启动完成后运行清理
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
 '(package-selected-packages '(catppuccin-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
