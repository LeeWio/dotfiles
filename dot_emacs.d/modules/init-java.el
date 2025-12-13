;;; init-java.el --- Java 开发配置
;; 参考 https://github.com/emacs-lsp/lsp-java

(require 'lsp-mode)
(require 'lsp-java)

;; Java LSP 配置
(setq lsp-java-vmargs
      '("-XX:+UseParallelGC"
        "-XX:GCTimeRatio=4"
        "-XX:AdaptiveSizePolicyWeight=90"
        "-Dsun.zip.disableMemoryMapping=true"
        "-Xmx2G"
        "-Xms100m"
        ;; Lombok 支持
        "-javaagent:/Users/wei.li/.emacs.d/lombok.jar"))

(setq lsp-java-format-enabled t)
(setq lsp-java-save-actions-organize-imports t)

;; Lombok 配置
(setq lsp-java-bundles
      (list (expand-file-name "~/.emacs.d/lombok.jar")))

;; 添加 hook
(add-hook 'java-mode-hook #'lsp)

(provide 'init-java)
;;; init-java.el ends here