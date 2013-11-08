
;; cscope-indexer
;; when source files are not under a directory, use it

(setq my-cscope-path (concat my-site-lisp-dir "xcscope/"))
(require 'xcscope)
;; Just load xcscope when open c/c++
;; (add-hook 'c-mode-common-hook
;;         '(lambda ()
;;             (require 'xcscope)))

(setq cscope-do-not-update-database t)
(setq cscope-indexing-script (concat my-cscope-path "cscope-indexer"))
(when win32p
    (if (file-directory-p "D:/Tool/cscope")
        (add-to-list 'exec-path "D:/Tool/cscope")))

;; C-c s I         cscope-index-files. Create list and index
;; C-c s a         cscope-set-init-directory. Set initial directory
;; C-c s A         Unset initial directory
;; C-c s L         Create list of files to index
;; C-c s E         Edit list of files to index
;; C-c s W         Locate this buffer's cscope directory. W->where
;; C-c s S         Locate this buffer's cscope directory. S->show
;; C-c s T         Locate this buffer's cscope directory. T->tell
;; C-c s D         Dired this buffer's directory
;; Search
;; C-c s s         Find symbol.
;; C-c s d         Find global definition.
;; C-c s g         Find global definition (alternate binding).
;; C-c s G         Find global definition without prompting.
;; C-c s c         Find functions calling a function.
;; C-c s C         Find called functions (list functions called
;;                 from a function).
;; C-c s t         Find text string.
;; C-c s e         Find egrep pattern.
;; C-c s f         Find a file.
;; C-c s i         Find files #including a file.)
;;
;; switch
;; C-c s b         Display *cscope* buffer.
;; C-c s B         Auto display *cscope* buffer toggle.
;; C-c s n         Next symbol.
;; C-c s N         Next file.
;; C-c s p         Previous symbol.
;; C-c s P         Previous file.
;; C-c s u         Pop mark.
