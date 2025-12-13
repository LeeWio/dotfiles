;;; init-benchmark.el --- 性能分析工具
;; 提供启动时间测量功能

(defun my/elapsed-time ()
  "计算从启动开始经过的时间。"
  (let ((elapsed (float-time (time-subtract (current-time) my/init-start-time))))
    elapsed))

(defmacro my/timed (name &rest body)
  "测量并记录代码执行时间。"
  `(let ((start-time (current-time))
         result)
     (setq result (progn ,@body))
     (message "[%s] 加载完成 (%.3fs)" ,name
              (float-time (time-subtract (current-time) start-time)))
     result))

(provide 'init-benchmark)
;;; init-benchmark.el ends here
