
(require 'slime)

(setq inferior-lisp-program "sbcl")

;; (ignore-errors (load (expand-file-name "~/.quicklisp/slime-helper.el")))

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol*-fancy t
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-when-complete-filename-expand t
           slime-truncate-lines nil
           slime-autodoc-use-multiline-p t)
     (setq slime-lisp-implementations
      '((clojure ("/usr/local/bin/lein" "repl"))
        (sbcl ("/usr/local/bin/sbcl"))))
     (global-set-key "\C-c \C-d d" 'slime-describe-symbol)
     (slime-setup)))

