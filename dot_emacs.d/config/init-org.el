;;; init-org.el --- Org-mode 配置
;; Copyright (C) 2025 Wei Li
;; Author: Wei Li
;; Version: 1.0
;; Keywords: org, outlines, hypermedia

;;; Commentary:
;; Org-mode 配置文件
;; 包含笔记、任务管理、文档编写等相关设置

;;; Code:

;; Org-mode 基本配置
(use-package org
  :ensure nil  ; org-mode 内置于 Emacs
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         ("C-c x" . org-export-dispatch))
  :config
  ;; 设置默认笔记目录
  (setq org-directory "~/org")
  
  ;; 设置默认文件
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  
  ;; 启用语法高亮
  (setq org-src-fontify-natively t)
  
  ;; 启用 LaTeX 预览
  (setq org-latex-preview-active t)
  
  ;; 设置任务状态
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  
  ;; 启用缩进模式
  (setq org-startup-indented t)
  
  ;; 启用数字列表
  (setq org-list-allow-alphabetical t)
  
  ;; 设置标签列位置
  (setq org-tags-column 0)
  
  ;; 启用图片预览
  (setq org-startup-with-inline-images t)
  
  ;; 设置表格公式调试
  (setq org-table-formula-debug t)
  
  ;; 启用广告窗口
  (setq org-startup-with-latex-preview t)
  
  ;; 设置导出设置
  (setq org-export-with-toc t)
  (setq org-export-with-section-numbers t)
  (setq org-export-with-smart-quotes t)
  (setq org-export-with-sub-superscripts t)
  
  ;; 导出到 PDF 设置
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  
  ;; 导出到 HTML 设置
  (setq org-html-validation-link nil)
  (setq org-html-head-include-scripts t)
  (setq org-html-head-include-default-style t)
  
  ;; 设置默认导出目录
  (setq org-export-directory "~/org/export/")
  
  ;; 设置优先级范围
  (setq org-priority-faces
        '((?A . (:foreground "red" :weight bold))
          (?B . (:foreground "orange" :weight bold))
          (?C . (:foreground "yellow" :weight bold))))
  
  ;; 启用打字机模式
  (setq org-use-speed-commands t)
  
  ;; 设置提醒
  (setq org-deadline-warning-days 7)
  
  ;; 设置字体化
  (setq org-hide-emphasis-markers t)
  
  ;; 设置折叠符号
  (setq org-ellipsis " ▾")
  
  ;; 设置标题级别字体
  (setq org-level-faces
        '(outline-1 outline-2 outline-3 outline-4 
          outline-5 outline-6 outline-7 outline-8))
  
  ;; 启用列表中的自动换行
  (setq org-return-follows-link t))

;; Org-babel 配置 - 代码块执行
(use-package org-contrib
  :ensure t
  :after org
  :config
  ;; 启用各种语言的代码块支持
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (shell . t)
          (python . t)
          (javascript . t)
          (css . t)
          (latex . t)
          (sql . t)
          (R . t)))
  
  ;; 执行代码块时不询问
  (setq org-confirm-babel-evaluate nil))

;; Org-agenda 配置
(use-package org-agenda
  :ensure nil
  :after org
  :config
  ;; 设置 agenda 文件
  (setq org-agenda-files (list org-directory))
  
  ;; 设置日视图
  (setq org-agenda-span 'day)
  
  ;; 启用日志模式
  (setq org-agenda-start-with-log-mode t)
  
  ;; 设置标签
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((agenda "")
            (alltodo "")))))

  ;; 设置时间戳格式
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          "......" "----------------"))

  ;; 启用过滤器
  (setq org-agenda-use-tag-inheritance t))

;; Org-capture 配置
(use-package org-capture
  :ensure nil
  :after org
  :config
  ;; 设置 capture 模板
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
           "* %? :NOTE:\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a"))))

(provide 'init-org)
;;; init-org.el ends here