;;; init-projectile.el --- Projectile 项目管理配置
;; Copyright (C) 2025 Wei Li
;; Author: Wei Li
;; Version: 1.0
;; Keywords: project, convenience

;;; Commentary:
;; Projectile 项目管理配置文件
;; 提供强大的项目导航和管理功能

;;; Code:

;; Projectile 包配置
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  ;; 设置项目搜索路径
  (setq projectile-project-search-path '("~/projects" "~/work" "~/code"))
  
  ;; 启用全局模式
  (projectile-mode +1)
  
  :config
  ;; 基本设置
  (setq projectile-enable-caching t)  ; 启用缓存提高性能
  (setq projectile-indexing-method 'alien)  ; 使用原生索引方法提高速度
  (setq projectile-sort-order 'recently-active)  ; 按最近活跃排序
  
  ;; 设置项目忽略文件
  (setq projectile-globally-ignored-files
        '(".DS_Store" "TAGS" "*.log" "*.min.js" "*.min.css"))
  
  ;; 设置项目忽略目录
  (setq projectile-globally-ignored-directories
        '(".idea" ".vscode" ".sass-cache" ".hg" ".git" "node_modules" 
          "bower_components" "dist" "build" "_build" "target" ".stack-work"))
  
  ;; 自定义项目根目录标识文件
  (setq projectile-project-root-files
        '("rebar.config" "project.clj" "pom.xml" "build.sbt" "build.gradle" 
          "Gemfile" "requirements.txt" "package.json" "setup.py" "tox.ini" 
          "composer.json" "Cargo.toml" "mix.exs" "stack.yaml" ".git" ".hg" 
          ".fslckout" ".bzr" "_darcs" ".svn" "Makefile" "configure.ac" 
          "configure.in" "Rakefile" "Vagrantfile" "Gruntfile.js" 
          "Gulpfile.js" "webpack.config.js" "gulpfile.babel.js" 
          "gruntfile.babel.js"))
  
  ;; 设置快捷键绑定
  :bind-keymap
  ("C-c p" . projectile-command-map)
  
  :bind (:map projectile-command-map
         ("C-p" . projectile-switch-project)
         ("C-f" . projectile-find-file)
         ("C-g" . projectile-find-file-dwim)
         ("C-d" . projectile-find-dir)
         ("C-b" . projectile-switch-to-buffer)
         ("C-s" . projectile-ripgrep)
         ("C-r" . projectile-replace)
         ("C-t" . projectile-toggle-between-implementation-and-test)
         ("!" . projectile-run-shell-command-in-root)
         ("&" . projectile-run-async-shell-command-in-root)))

;; 与 Ivy 集成（如果已安装）
(use-package counsel-projectile
  :ensure t
  :defer t
  :after (projectile counsel)
  :config
  (counsel-projectile-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here