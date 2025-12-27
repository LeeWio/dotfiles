;;; frontend-project.el --- Frontend project configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Frontend project management and utilities

;;; Code:

;; Configure project management for frontend projects
(use-package project
  :config
  ;; Recognize common frontend project root markers
  (add-to-list 'project-find-functions 'project-try-vcs))

(provide 'frontend-project)
;;; frontend-project.el ends here