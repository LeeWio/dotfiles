;;; mode-line-config.el --- Modern VS Code style mode line -*- lexical-binding: t -*-

;;; Commentary:
;; Modern VS Code style mode line with improved icons and layout

;;; Code:

;; Install and configure nerd-icons for mode line
(use-package nerd-icons
  :ensure t)

;; Editor color palette (Catppuccin Mocha)
(defconst my/editor-colors
  '((background    . "#1e1e2e")    ; Catppuccin base
    (inactive-bg   . "#181825")    ; Catppuccin mantle
    (foreground    . "#cdd6f4")    ; Catppuccin text
    (inactive-fg   . "#6c7086")    ; Catppuccin overlay0
    (blue          . "#89b4fa")    ; Catppuccin blue
    (green         . "#a6e3a1")    ; Catppuccin green
    (yellow        . "#f9e2af")    ; Catppuccin yellow
    (red           . "#f38ba8")    ; Catppuccin red
    (purple        . "#cba6f7")    ; Catppuccin mauve
    (orange        . "#fab387")    ; Catppuccin peach
    (teal          . "#94e2d5")    ; Catppuccin teal
    (surface0      . "#313244")    ; Catppuccin surface0
    (surface1      . "#45475a")))  ; Catppuccin surface1

;; Helper function to get colors
(defun my/editor-color (color-name)
  "Get editor COLOR-NAME."
  (cdr (assoc color-name my/editor-colors)))

;; File type icons with nerd-icons
(defun my/file-icon (filename)
  "Get file icon for FILENAME with nerd-icons."
  (when (require 'nerd-icons nil t)
    (let ((ext (and filename (downcase (file-name-extension filename)))))
      (cond
       ;; Programming languages
       ((member ext '("el" "lisp")) (nerd-icons-codicon "nf-cod-symbol-constructor" :face 'nerd-icons-purple))
       ((member ext '("js" "jsx")) (nerd-icons-devicon "nf-dev-javascript" :face 'nerd-icons-yellow))
       ((member ext '("ts" "tsx")) (nerd-icons-devicon "nf-dev-typescript" :face 'nerd-icons-blue))
       ((member ext '("py")) (nerd-icons-devicon "nf-dev-python" :face 'nerd-icons-red))
       ((member ext '("html" "htm")) (nerd-icons-devicon "nf-dev-html5" :face 'nerd-icons-orange))
       ((member ext '("css" "scss" "sass")) (nerd-icons-devicon "nf-dev-css3" :face 'nerd-icons-blue))
       ((member ext '("json")) (nerd-icons-codicon "nf-cod-json" :face 'nerd-icons-yellow))
       ((member ext '("md" "markdown")) (nerd-icons-mdicon "nf-md-file_document" :face 'nerd-icons-blue))
       ((member ext '("java")) (nerd-icons-devicon "nf-dev-java" :face 'nerd-icons-purple))
       ((member ext '("c" "cpp" "h" "hpp")) (nerd-icons-devicon "nf-dev-c" :face 'nerd-icons-blue))
       ((member ext '("php")) (nerd-icons-devicon "nf-dev-php" :face 'nerd-icons-purple))
       ((member ext '("rb")) (nerd-icons-devicon "nf-dev-ruby" :face 'nerd-icons-red))
       ((member ext '("go")) (nerd-icons-devicon "nf-dev-go" :face 'nerd-icons-blue))
       ((member ext '("rs")) (nerd-icons-devicon "nf-dev-rust" :face 'nerd-icons-orange))
       
       ;; Configuration
       ((member ext '("yaml" "yml")) (nerd-icons-codicon "nf-cod-settings" :face 'nerd-icons-yellow))
       ((member ext '("xml")) (nerd-icons-devicon "nf-dev-xml" :face 'nerd-icons-red))
       ((member ext '("toml")) (nerd-icons-codicon "nf-cod-settings" :face 'nerd-icons-purple))
       ((member ext '("conf" "cfg")) (nerd-icons-codicon "nf-cod-settings" :face 'nerd-icons-orange))
       
       ;; Documentation
       ((member ext '("txt" "text")) (nerd-icons-faicon "nf-fa-file_text_o" :face 'nerd-icons-green))
       ((member ext '("org")) (nerd-icons-faicon "nf-fa-list_alt" :face 'nerd-icons-purple))
       ((member ext '("pdf")) (nerd-icons-codicon "nf-cod-file_pdf" :face 'nerd-icons-red))
       ((member ext '("tex")) (nerd-icons-faicon "nf-fa-file_text" :face 'nerd-icons-yellow))
       
       ;; Media
       ((member ext '("jpg" "jpeg" "png" "gif" "bmp" "svg")) (nerd-icons-faicon "nf-fa-file_image_o" :face 'nerd-icons-green))
       ((member ext '("mp3" "wav" "ogg" "flac")) (nerd-icons-faicon "nf-fa-file_audio_o" :face 'nerd-icons-green))
       ((member ext '("mp4" "avi" "mov" "mkv")) (nerd-icons-faicon "nf-fa-file_video_o" :face 'nerd-icons-green))
       
       ;; Archives
       ((member ext '("zip" "tar" "gz" "rar" "7z")) (nerd-icons-faicon "nf-fa-file_archive_o" :face 'nerd-icons-orange))
       
       ;; Scripts
       ((member ext '("sh" "bash" "zsh")) (nerd-icons-devicon "nf-dev-terminal" :face 'nerd-icons-purple))
       
       ;; Default file icon
       (filename (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-blue))
       (t (nerd-icons-faicon "nf-fa-file" :face 'nerd-icons-blue))))))

;; Mode icons with nerd-icons
(defun my/mode-icon (mode)
  "Get icon for MODE with nerd-icons."
  (when (require 'nerd-icons nil t)
    (cond
     ((eq mode 'dired-mode) (nerd-icons-faicon "nf-fa-folder_o" :face 'nerd-icons-orange))  ; Folder
     ((eq mode 'org-mode) (nerd-icons-faicon "nf-fa-list_alt" :face 'nerd-icons-purple))    ; Org
     ((eq mode 'python-mode) (nerd-icons-devicon "nf-dev-python" :face 'nerd-icons-red))    ; Python
     ((eq mode 'js-mode) (nerd-icons-devicon "nf-dev-javascript" :face 'nerd-icons-yellow)) ; JavaScript
     ((eq mode 'typescript-mode) (nerd-icons-devicon "nf-dev-typescript" :face 'nerd-icons-blue)) ; TypeScript
     ((eq mode 'html-mode) (nerd-icons-devicon "nf-dev-html5" :face 'nerd-icons-orange))    ; HTML
     ((eq mode 'css-mode) (nerd-icons-devicon "nf-dev-css3" :face 'nerd-icons-blue))        ; CSS
     ((eq mode 'text-mode) (nerd-icons-faicon "nf-fa-file_text_o" :face 'nerd-icons-green)) ; Text
     ((eq mode 'fundamental-mode) (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-blue)) ; Basic
     (t (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-blue)))))                      ; Default

;; Custom mode line faces - clean VS Code style
(set-face-attribute 'mode-line nil
                    :background (my/editor-color 'background)
                    :foreground (my/editor-color 'foreground)
                    :box `(:line-width (1 . 1) :color ,(my/editor-color 'surface0))
                    :underline nil
                    :overline nil
                    :inherit nil)
(set-face-attribute 'mode-line-inactive nil
                    :background (my/editor-color 'inactive-bg)
                    :foreground (my/editor-color 'inactive-fg)
                    :box `(:line-width (1 . 1) :color ,(my/editor-color 'inactive-bg))
                    :underline nil
                    :overline nil
                    :inherit nil)

;; Modern VS Code style mode line format
(setq-default mode-line-format
  '(""
    ;; Left: File/buffer icon and name
    (:eval 
     (let* ((filename (buffer-file-name))
            (icon (if filename
                      (my/file-icon filename)
                    (my/mode-icon major-mode)))
            (name (if filename
                      (file-name-nondirectory filename)
                    (buffer-name))))
       (concat
        " "
        (propertize icon 'face `(:foreground ,(my/editor-color 'blue)))
        " "
        (propertize name 'face `(:weight bold :foreground ,(my/editor-color 'blue)))
        (when (buffer-modified-p)
          (concat " " (propertize "●" 'face `(:foreground ,(my/editor-color 'red)))))
        " ")))
    
    ;; Center: Mode information
    (:eval 
     (let ((mode-name-str (format-mode-line mode-name)))
       (concat
        " "
        (propertize (cond
                     ((string-match "dired" mode-name-str) "DIR")
                     ((string-match "org" mode-name-str) "ORG")
                     ((string-match "text" mode-name-str) "TXT")
                     ((string-match "prog" mode-name-str) "CODE")
                     ((> (length mode-name-str) 10) 
                      (concat (substring mode-name-str 0 7) "..."))
                     (t mode-name-str))
                    'face `(:foreground ,(my/editor-color 'purple)))
        " ")))
    
    ;; Right: Position and status
    (:eval 
     (concat
      ;; Flexible spacing
      (make-string (max 0 (- (window-width) 50)) ?\s)
      ;; Line/column position
      (propertize "%l:%c" 'face `(:foreground ,(my/editor-color 'yellow)))
      " "
      ;; Percentage through file
      (propertize "%p" 'face `(:foreground ,(my/editor-color 'green)))
      "  "))))

(provide 'mode-line-config)
;;; mode-line-config.el ends here