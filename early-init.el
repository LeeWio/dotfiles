;;; early-init.el --- Emacs 启动早期优化
;; 禁用不需要的启动功能来提高性能

;; 禁用包管理系统直到我们准备好了
(setq package-enable-at-startup nil)

;; 禁用图形元素加快启动
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 延迟加载 GUI 组件
(setq frame-inhibit-implied-resize t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; 优化 GC 以加速启动
(setq gc-cons-threshold 100000000)  ;; 100MB
(setq read-process-output-max (* 1024 1024))  ;; 1MB
