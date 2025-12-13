;;; init-org.el --- Org Mode 增强配置

;; Org Mode 基础配置
(use-package org
  :ensure t
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  ;; 基础设置
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list org-directory))
  
  ;; 外观设置
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-pretty-entities t)
  (setq org-use-sub-superscripts "{}")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  
  ;; TODO 关键字
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  
  ;; TODO 颜色
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#f38ba8" :weight bold))
          ("IN-PROGRESS" . (:foreground "#89b4fa" :weight bold))
          ("WAITING" . (:foreground "#f9e2af" :weight bold))
          ("DONE" . (:foreground "#a6e3a1" :weight bold))
          ("CANCELLED" . (:foreground "#6c7086" :weight bold))))
  
  ;; 优先级
  (setq org-priority-faces
        '((?A . (:foreground "#f38ba8" :weight bold))
          (?B . (:foreground "#f9e2af" :weight bold))
          (?C . (:foreground "#89b4fa" :weight bold))))
  
  ;; 快速捕获模板
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline "" "Notes")
           "* %?\n  %i\n  %U")
          ("j" "Journal" entry (file+datetree "journal.org")
           "* %?\nEntered on %U\n  %i")))
  
  ;; Agenda 视图
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "In Progress")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Upcoming Tasks")))))))
  
  ;; 代码块快捷方式
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("java" . "src java"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript")))

;; Org 美化
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0.2)
  (setq org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•")))
  (setq org-modern-block-fringe t)
  (setq org-modern-keyword t)
  (setq org-modern-checkbox
        '((88 . "✓")
          (45 . "–")
          (32 . "○")))
  (setq org-modern-todo t)
  (setq org-modern-tag t)
  (setq org-modern-timestamp t))

;; Org 子弹点美化
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "✤" "✜" "◆")))

;; Org Roam（可选 - 知识库管理）
;; (use-package org-roam
;;   :ensure t
;;   :custom
;;   (org-roam-directory "~/org/roam")
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n i" . org-roam-node-insert))
;;   :config
;;   (org-roam-setup))

(provide 'init-org)
;;; init-org.el ends here
