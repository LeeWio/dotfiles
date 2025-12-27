;;; org-config.el --- Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org mode configuration for note-taking, document authoring, and task management

;;; Code:

;; ------------------------------------------------------------------
;; Ensure org is loaded
;; ------------------------------------------------------------------
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

;; ------------------------------------------------------------------
;; Basic org configuration
;; ------------------------------------------------------------------
(use-package org
  :ensure t
  :defer t
  :config
  ;; Set default org directory
  (setq org-directory "~/org")

  ;; Set default notes file
  (setq org-default-notes-file
        (concat org-directory "/notes.org"))

  ;; Configure org-agenda files
  (setq org-agenda-files
        (list org-directory))

  ;; Enable org-babel support for common languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (emacs-lisp . t)
     (sql . t)))

  ;; Disable confirmation when evaluating code blocks
  (setq org-confirm-babel-evaluate nil)

  ;; Configure TODO keywords
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "IN-PROGRESS(i)"
           "WAITING(w@/!)"
           "|"
           "DONE(d)"
           "CANCELLED(c@)")))

  ;; Appearance settings
  (setq org-startup-indented t)
  (setq org-startup-folded 'content)
  (setq org-ellipsis " ▼ ")

  ;; Structure editing settings
  (setq org-adapt-indentation t)

  ;; Archive settings
  (setq org-archive-location
        (concat org-directory "/archive.org::")))

;; ------------------------------------------------------------------
;; Key bindings for org mode
;; ------------------------------------------------------------------
(use-package org
  :bind (:map org-mode-map
              ("C-c l" . org-store-link)
              ("C-c a" . org-agenda)
              ("C-c c" . org-capture)))

;; ------------------------------------------------------------------
;; Org capture templates
;; ------------------------------------------------------------------
(with-eval-after-load 'org
  (setq org-capture-templates
        '(("t" "Todo"
           entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")

          ("n" "Note"
           entry
           (file+headline org-default-notes-file "Notes")
           "* %? :NOTE:\n  %i\n  %a")

          ("j" "Journal"
           entry
           (file+olp (concat org-directory "/journal.org")
                     "Journal" "Today")
           "* %U - %?\n  %i\n  %a"))))

;; ------------------------------------------------------------------
;; Org contrib for extra functionality (if available)
;; ------------------------------------------------------------------
(use-package org-contrib
  :ensure t
  :defer t
  :after org)

(provide 'org-config)

;;; org-config.el ends here
