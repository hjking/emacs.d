
(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-when-complete-filename-expand t
           slime-truncate-lines nil
           slime-autodoc-use-multiline-p t
           inferior-lisp-program "sbcl")
     (setq slime-lisp-implementations
      '((clojure ("/usr/local/bin/lein" "repl"))
        (sbcl ("/usr/local/bin/sbcl"))))
     (global-set-key "\C-c \C-d d" 'slime-describe-symbol)
     (slime-setup)))
