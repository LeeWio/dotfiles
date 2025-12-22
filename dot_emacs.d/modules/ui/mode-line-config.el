;;; mode-line-config.el --- Modern VS Code style mode line -*- lexical-binding: t -*-

;;; Commentary:
;; Modern VS Code style mode line with improved icons and layout

;;; Code:

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

;; File type icons with clearer representations
(defun my/file-icon (filename)
  "Get file icon for FILENAME with better visual representation."
  (let ((ext (and filename (downcase (file-name-extension filename)))))
    (cond
     ;; Programming languages
     ((member ext '("el" "lisp")) "𝛌")      ; Lambda for Lisp
     ((member ext '("js" "jsx")) "🅹")       ; J for JavaScript
     ((member ext '("ts" "tsx")) "🆃")       ; T for TypeScript
     ((member ext '("py")) "🐍")             ; Snake for Python
     ((member ext '("html" "htm")) "🅷")     ; H for HTML
     ((member ext '("css" "scss" "sass")) "🅲") ; C for CSS
     ((member ext '("json")) "{ }")          ; Brackets for JSON
     ((member ext '("md" "markdown")) "🅼")  ; M for Markdown
     ((member ext '("java")) "☕")           ; Coffee for Java
     ((member ext '("c" "cpp" "h" "hpp")) "🅒") ; C for C/C++
     ((member ext '("php")) "🅟")            ; P for PHP
     ((member ext '("rb")) "🅡")             ; R for Ruby
     ((member ext '("go")) "🅶")             ; G for Go
     ((member ext '("rs")) "🆁")             ; R for Rust
     
     ;; Configuration
     ((member ext '("yaml" "yml")) "🅨")    ; Y for YAML
     ((member ext '("xml")) "🅨")            ; Y for XML
     ((member ext '("toml")) "🅣")           ; T for TOML
     ((member ext '("conf" "cfg")) "⚙")     ; Gear for config
     
     ;; Documentation
     ((member ext '("txt" "text")) "🅣")    ; T for Text
     ((member ext '("org")) "Ⓞ")            ; O for Org
     ((member ext '("pdf")) "🅟")            ; P for PDF
     ((member ext '("tex")) "🅛")            ; L for LaTeX
     
     ;; Media
     ((member ext '("jpg" "jpeg" "png" "gif" "bmp" "svg")) "🖼") ; Image
     ((member ext '("mp3" "wav" "ogg" "flac")) "🎵")            ; Music
     ((member ext '("mp4" "avi" "mov" "mkv")) "🎬")             ; Movie
     
     ;; Archives
     ((member ext '("zip" "tar" "gz" "rar" "7z")) "📦")        ; Package
     
     ;; Scripts
     ((member ext '("sh" "bash" "zsh")) "$")                   ; Dollar for shell
     
     ;; Default file icon
     (filename "📄")  ; Generic file
     (t "📝"))))     ; Buffer (no file)

;; Mode icons
(defun my/mode-icon (mode)
  "Get icon for MODE."
  (cond
   ((eq mode 'dired-mode) "📁")      ; Folder
   ((eq mode 'org-mode) "Ⓞ")         ; Org
   ((eq mode 'python-mode) "🐍")     ; Python
   ((eq mode 'js-mode) "🅹")          ; JavaScript
   ((eq mode 'typescript-mode) "🆃")  ; TypeScript
   ((eq mode 'html-mode) "🅷")        ; HTML
   ((eq mode 'css-mode) "🅲")         ; CSS
   ((eq mode 'text-mode) "🆃")        ; Text
   ((eq mode 'fundamental-mode) "📄") ; Basic
   (t "📄")))                         ; Default

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