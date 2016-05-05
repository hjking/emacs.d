
(message "%d: >>>>> Loading [ TAGS ] Customization ...." step_no)
(setq step_no (1+ step_no))
;;; ctags -e -R *.cpp *.h
;;; M-. : find-tag ;
;;; M-* : jump back ;
;;; M-x tags-search : regexp-search
;;; M-, : resume 'tags-search'
;;; M-x tags-apropos : list all tags in a tag file that match a regexp
;;; M-x list-tags : list all tags defined in a source file
;;; C-x 4 . tag : Find first definition of tag, but display it in another window (find-tag-other-window).
;;; C-x 5 . tag : Find first definition of tag, and create a new frame to select the buffer (find-tag-other-frame).
;; (setq path-to-ctags "/usr/bin/ctags")

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold 50000000)  ;; nil

(when *is-a-mac*
  ; Mac's default ctags does not support -e option
  ; If you install Emacs by homebrew, another version of etags is already installed which does not need -e too
  (setq ctags-command "/usr/local/bin/ctags -e -R ") ;; the best option is to install latest ctags from sf.net
  )

;; From http://blog.binchen.org/?p=1057
;; I hard coded full path of TAGS in .emacs because I usually don’t change project path.
;; In major mode hook like c++-mode-hook or js2-mode-hook I will check the directory path of current file. If it contains certain string, I suppose the file belong to certain project.
;; Then I will create TAGS for that project if needed
;; Every time when I save the file, I may update TAGS according to the value of tags-table-list.

;; set search dirs
; (setq tags-table-list '("./TAGS" "../TAGS" "../.."))

(defun my-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))

(defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
  "return the full path of tags file"
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)) )
       file)
    (setq file (concat dir "TAGS"))
    (when (or FORCE (not (file-exists-p file)))
      (message "Creating TAGS in %s ..." dir)
      (shell-command
       (format "ctags -f %s -R %s" file dir))
      )
    file
    ))

(defvar my-tags-updated-time nil)

(defun my-update-tags ()
  (interactive)
  "check the tags in tags-table-list and re-create it"
  (dolist (tag tags-table-list)
    (my-create-tags-if-needed (file-name-directory tag) t)
    ))

(defun my-auto-update-tags-when-save ()
  (interactive)
  (cond
   ((not my-tags-updated-time)
    (setq my-tags-updated-time (current-time)))
   ((< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300)
    ;; < 300 seconds
    ;; do nothing
    )
   (t
    (setq my-tags-updated-time (current-time))
    (my-update-tags)
    (message "updated tags after %d seconds." (- (float-time (current-time))  (float-time my-tags-updated-time)))
    )
   ))

(defun my-setup-develop-environment ()
    (when (my-project-name-contains-substring "hongjin")
      (cond
       ((my-project-name-contains-substring "systemc")
        ;; C++ project don't need html tags
        (setq tags-table-list (list
                               (my-create-tags-if-needed
                                (concat (file-name-as-directory (getenv "SYSTEMC_HOME")) "include"))
                               (my-create-tags-if-needed "~/hongjin/workspace/fabric/software/systemc")))
        )
       )))

; (add-hook 'after-save-hook 'my-auto-update-tags-when-save)
; (add-hook 'c++-mode-hook 'my-setup-develop-environment)
; (add-hook 'c-mode-hook 'my-setup-develop-environment)

(defun build-ctags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gtags
;;
(defun gtags-ext-produce-tags-if-needed (dir)
   (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((olddir default-directory))
        (cd dir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir)) ; restore
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'"))
  )

;; @see http://emacs-fu.blogspot.com.au/2008/01/navigating-through-source-code-using.html
(defun gtags-ext-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (gtags-ext-produce-tags-if-needed (read-directory-name
                            "gtags: top of source tree:" default-directory)))

(defun gtags-ext-add-gtagslibpath (libdir &optional del)
  "add external library directory to environment variable GTAGSLIBPATH.\ngtags will can that directory if needed.\nC-u M-x add-gtagslibpath will remove the directory from GTAGSLIBPATH."
  (interactive "DDirectory containing GTAGS:\nP")
  (let (sl)
  (if (not (file-exists-p (concat (file-name-as-directory libdir) "GTAGS")))
      ;; create tags
      (let ((olddir default-directory))
        (cd libdir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir)
        )
    )
  (setq libdir (directory-file-name libdir)) ;remove final slash
  (setq sl (split-string (if (getenv "GTAGSLIBPATH") (getenv "GTAGSLIBPATH") "")  ":" t))
  (if del (setq sl (delete libdir sl)) (add-to-list 'sl libdir t))
  (setenv "GTAGSLIBPATH" (mapconcat 'identity sl ":")))
  )

(defun gtags-ext-print-gtagslibpath ()
  "print the GTAGSLIBPATH (for debug purpose)"
  (interactive)
  (message "GTAGSLIBPATH=%s" (getenv "GTAGSLIBPATH"))
  )

; (message "%d: >>>>> Loading [ ggtags ] Customization ...." step_no)
; (require 'ggtags)
; (add-hook 'c-mode-common-hook
;           (lambda ()
;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;               (ggtags-mode 1))))

; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)