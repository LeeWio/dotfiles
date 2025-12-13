;;; early-init.el --- Emacs 启动早期优化

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
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)

;; 优化 GC 以加速启动
(setq gc-cons-threshold most-positive-fixnum)  ;; 启动时禁用GC
(setq gc-cons-percentage 0.6)

;; 增加读取进程输出的最大值
(setq read-process-output-max (* 3 1024 1024))  ;; 3MB

;; 禁用不必要的功能
(setq inhibit-compacting-font-caches t)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; 减少启动时的正则表达式查找
(setq auto-mode-case-fold nil)

;;; early-init.el ends here