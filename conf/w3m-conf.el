;;; w3m-conf.el ---
;;
(use-package w3m
  :load-path (lambda () (concat my-site-lisp-dir "emacs-w3m/"))
  ; :bind (("C-c w m"  .  browse-url-at-point)
  ;        ("C-c w p"  .  w3m-browse-current-buffer)
  ;        ("C-c w b"  .  browse-url))
  :commands (w3m-browse-url)
  :init (progn
          (setq browse-url-browser-function 'w3m-browse-url
                w3m-coding-system 'utf-8
                w3m-file-coding-system 'utf-8
                w3m-file-name-coding-system 'utf-8
                w3m-input-coding-system 'utf-8
                w3m-output-coding-system 'utf-8
                w3m-terminal-coding-system 'utf-8
                w3m-use-cookies t
                w3m-cookie-accept-bad-cookies t
                w3m-display-inline-image t))
  :config (progn

      (add-hook 'w3m-display-hook
                (lambda (url)
                  (rename-buffer
                   (format "*w3m: %s*" (or w3m-current-title
                                           w3m-current-url)) t)))

      (defun w3m-open-current-page-in-firefox ()
        "Opens the current URL in Mozilla Firefox."
        (interactive)
        (browse-url-firefox w3m-current-url))

      (defun w3m-open-link-or-image-in-firefox ()
        "Opens the current link or image in Firefox."
        (interactive)
        (browse-url-firefox (or (w3m-anchor)
                                (w3m-image))))

      ; (setq w3m-search-default-engine "g")
      ; (with-eval-after-load 'w3m-search
      ;    ; C-u S g RET <search term> RET
      ;    (add-to-list 'w3m-search-engine-alist '("g" "http://www.google.com.au/search?hl=en&q=%s" utf-8))
      ;    (add-to-list 'w3m-search-engine-alist '("wz" "http://zh.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
      ;    (add-to-list 'w3m-search-engine-alist '("q" "http://www.google.com.au/search?hl=en&q=%s+site:stackoverflow.com" utf-8))
      ;    (add-to-list 'w3m-search-engine-alist '("s" "http://code.ohloh.net/search?s=%s&browser=Default"  utf-8))
      ;    (add-to-list 'w3m-search-engine-alist '("b" "http://blogsearch.google.com.au/blogsearch?q=%s" utf-8))
      ;    (add-to-list 'w3m-search-engine-alist '("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
      ;    (add-to-list 'w3m-search-engine-alist '("d" "http://dictionary.reference.com/search?q=%s" utf-8))
      ;    (add-to-list 'w3m-search-engine-alist '("j" "http://www.google.com.au/search?ie=UTF-8&oe=UTF-8&sourceid=navclient&btnI=1&q=%s+site:developer.mozilla.org" utf-8))
      ;    )
    )
  )

(provide 'w3m-conf)