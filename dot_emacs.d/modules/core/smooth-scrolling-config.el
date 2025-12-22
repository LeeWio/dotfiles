;;; smooth-scrolling-config.el --- Final optimized smooth scrolling -*- lexical-binding: t -*-

;;; Commentary:
;; Final optimized smooth scrolling with essential enhancements

;;; Code:

;; Basic scrolling settings for smooth behavior
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq auto-window-vscroll nil)
(setq scroll-preserve-screen-position 'always)

;; Mouse wheel optimization
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; Enable pixel scrolling for modern Emacs
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))

;; ==================== 核心优化 ====================

;; 连击加速变量
(defvar my/last-scroll-time 0 "Timestamp of last scroll.")
(defvar my/scroll-acceleration 1.0 "Scroll acceleration factor.")

;; 更新滚动加速
(defun my/update-scroll-acceleration ()
  "Update scroll acceleration based on timing."
  (let ((current-time (float-time)))
    (if (< (- current-time my/last-scroll-time) 0.4)
        (setq my/scroll-acceleration (min 2.5 (+ my/scroll-acceleration 0.1)))
      (setq my/scroll-acceleration 1.0))
    (setq my/last-scroll-time current-time)))

;; 智能滚动量计算
(defun my/intelligent-scroll-lines ()
  "Calculate scroll lines based on window context."
  (let ((window-height (window-body-height)))
    (cond
     ((> window-height 60) 20)  ; 大窗口固定20行
     ((> window-height 40) 15)  ; 中窗口15行
     ((> window-height 25) 10)  ; 小窗口10行
     (t 5))))  ; 很小窗口5行

;; 动态延迟计算
(defun my/dynamic-delay (position total)
  "Calculate dynamic delay for natural scrolling feel."
  (let ((progress (if (= total 0) 0 (/ (float position) (float total)))))
    (cond
     ((< progress 0.15) 0.015)  ; 开始稍慢
     ((> progress 0.85) 0.015)  ; 结束稍慢
     (t 0.008))))  ; 中间较快

;; ==================== 优化的滚动函数 ====================
(defun my/smooth-scroll-down-enhanced ()
  "Enhanced smooth scroll down with acceleration and dynamic timing."
  (interactive)
  (my/update-scroll-acceleration)
  (condition-case nil
      (unless (pos-visible-in-window-p (point-max))
        (let* ((base-lines (my/intelligent-scroll-lines))
               (actual-lines (max 1 (floor (/ base-lines my/scroll-acceleration)))))
          (dotimes (i actual-lines)
            (condition-case nil
                (progn
                  (scroll-up 1)
                  (sit-for (my/dynamic-delay i actual-lines)))
              (error (return))))))
    (error nil)))

(defun my/smooth-scroll-up-enhanced ()
  "Enhanced smooth scroll up with acceleration and dynamic timing."
  (interactive)
  (my/update-scroll-acceleration)
  (condition-case nil
      (unless (pos-visible-in-window-p (point-min))
        (let* ((base-lines (my/intelligent-scroll-lines))
               (actual-lines (max 1 (floor (/ base-lines my/scroll-acceleration)))))
          (dotimes (i actual-lines)
            (condition-case nil
                (progn
                  (scroll-down 1)
                  (sit-for (my/dynamic-delay i actual-lines)))
              (error (return))))))
    (error nil)))

;; ==================== 键绑定 ====================
(global-set-key (kbd "C-v") 'my/smooth-scroll-down-enhanced)
(global-set-key (kbd "M-v") 'my/smooth-scroll-up-enhanced)

(provide 'smooth-scrolling-config)