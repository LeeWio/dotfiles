;;; init.el --- Emacs 配置入口文件

;; Copyright (C) 2025 Wei Li

;; Author: Wei Li
;; Version: 1.0
;; Keywords: convenience

;; 本文件是您 Emacs 配置的入口点
;; 它会加载各个模块化配置文件

;;; Commentary:

;; 这是一个模块化的 Emacs 配置框架
;; 每个功能模块都有独立的配置文件，便于维护和调试
;; 配置按需加载，优化启动性能

;;; Code:

;; 性能优化：延迟加载非必要功能
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

;; 设置垃圾回收阈值以提高启动速度
(setq gc-cons-threshold (* 100 1024 1024)) ; 100MB
(setq gc-cons-percentage 0.6)

;; 禁用不必要的警告和消息
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; 保存初始加载时间
(defvar emacs-start-time (current-time))

;; 添加自定义目录到加载路径
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/config/languages")

;; 禁用包加载时的初始化以提升性能
(setq package-enable-at-startup nil)

;; 包管理配置（主题需要优先加载）
(require 'init-packages)    ; 包管理器

;; 核心配置模块
;; (require 'init-core)        ; 基础配置

;; 界面配置模块
(require 'init-ui)          ; 用户界面

;; 编辑配置模块
(require 'init-editing)     ; 编辑增强

;; 快捷键配置模块
;; (require 'init-keybindings) ; 键绑定

;; 编程语言支持模块
(require 'init-languages)   ; 语言支持

;; 补全系统模块
(require 'init-completion)  ; 补全系统

;; 滚动配置模块
(require 'init-scrolling)   ; 滚动增强

;; Git 集成模块
(require 'init-git)         ; Git 集成

;; 工具配置模块
;; (require 'init-tools)       ; 实用工具

;; 项目管理模块
;; (require 'init-project)     ; 项目管理

;; 自定义配置（最后加载）
;; (require 'init-local)       ; 本地个性化配置

;; 恢复文件处理器和垃圾回收设置
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq file-name-handler-alist file-name-handler-alist-old)
   (setq gc-cons-threshold (* 20 1024 1024)) ; 恢复为 20MB
   (setq gc-cons-percentage 0.1)))

;; 显示启动时间
(add-hook 'after-init-hook
          (lambda ()
            (let ((elapsed (float-time (time-subtract (current-time)
                                                      emacs-start-time))))
              (message "Emacs 启动完成，耗时 %.3fs" elapsed))))

(provide 'init)

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
 )
