;;; init-markdown.el --- Markdown 和文档配置

;; Markdown 模式
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-italic-underscore t)
  (setq markdown-make-gfm-checkboxes-buttons t)
  (setq markdown-gfm-additional-languages '("sh" "java" "javascript" "typescript"))
  (setq markdown-header-scaling t)
  
  ;; 自动插入 TOC
  (setq markdown-toc-header-toc-start "<!-- TOC -->")
  (setq markdown-toc-header-toc-end "<!-- /TOC -->"))

;; Markdown TOC 生成
(use-package markdown-toc
  :ensure t
  :after markdown-mode)

;; Markdown 预览
(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :config
  (setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css")))

(provide 'init-markdown)
;;; init-markdown.el ends here
