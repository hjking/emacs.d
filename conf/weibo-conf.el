
(use-package weibo
  :load-path (lambda () (concat my-site-lisp-dir "weibo/"))
  :commands (weibo-timeline)
  :init (progn
    ;display no image
    (setq weibo-display-image nil)
    )
  )