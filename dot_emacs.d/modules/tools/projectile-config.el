;;; projectile-config.el --- Projectile configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Projectile project management configuration

;;; Code:

;; Install projectile if not already installed
(unless (package-installed-p 'projectile)
  (package-refresh-contents)
  (package-install 'projectile))

;; Load projectile
(require 'projectile)

;; Enable projectile mode
(projectile-mode 1)

;; Function to set current directory as a project
(defun my/projectile-set-current-dir-as-project ()
  "Force Projectile to treat the current directory as a known project."
  (interactive)
  (let ((root (expand-file-name default-directory)))
    ;; Add current directory to known projects
    (add-to-list 'projectile-known-projects root)
    ;; Save the known projects list to persist across sessions
    (projectile-save-known-projects)
    (message "Projectile now treats %s as a project" root)))

;; Set up key bindings for projectile
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Add the command to projectile-command-map so C-c p . works
(with-eval-after-load 'projectile
  ;; Define the key after projectile-command-map is fully loaded
  (define-key projectile-command-map (kbd ".") 'my/projectile-set-current-dir-as-project))

;; Automatically detect current directory as project when opening Emacs with a directory
(add-hook 'find-file-hook
          (lambda ()
            (when (and (not (projectile-project-p))
                       (file-directory-p default-directory))
              (let ((dir (expand-file-name default-directory)))
                (unless (member dir projectile-known-projects)
                  (add-to-list 'projectile-known-projects dir)
                  (projectile-save-known-projects))))))

;; Configure projectile settings
(setq projectile-enable-caching t)                    ; Enable caching for faster project operations
(setq projectile-kill-buffers-filter 'kill-only-buffer) ; Kill only buffer, not process
(setq projectile-sort-order 'recentf)                 ; Sort projects by recent files
(setq projectile-switch-project-action 'projectile-dired) ; Default action when switching projects
(setq projectile-require-project-root t)              ; Require project root for most operations
(setq projectile-use-git-grep t)                      ; Use git grep when available
(setq projectile-indexing-method 'hybrid)             ; Use hybrid indexing method
(setq projectile-globally-ignored-directories
      '(".git" ".svn" ".hg" ".bzr" "_darcs" "node_modules" ".idea" ".vscode" ".ccls-cache")) ; Ignore common directories
(setq projectile-globally-ignored-files
      '(".DS_Store" "*.elc" "*.pyc" "*.o" "*.class" "*.bin" ".gitignore")) ; Ignore common files

(provide 'projectile-config)
;;; projectile-config.el ends here