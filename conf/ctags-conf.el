
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
(setq path-to-ctags "ctags")

;; From http://blog.binchen.org/?p=1057
;; I hard coded full path of TAGS in .emacs because I usually don’t change project path.
;; In major mode hook like c++-mode-hook or js2-mode-hook I will check the directory path of current file. If it contains certain string, I suppose the file belong to certain project.
;; Then I will create TAGS for that project if needed
;; Every time when I save the file, I may update TAGS according to the value of tags-table-list.

;; set search dirs
; (setq tags-table-list '("./TAGS" "../TAGS" "../.."))
; (setq tags-table-list '("~/wxWidgets-master/TAGS" "~/projs/Loris/src/desktop/TAGS"))

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
    (when (my-project-name-contains-substring "Loris")
      (cond
       ((my-project-name-contains-substring "src/desktop")
        ;; C++ project don't need html tags
        (setq tags-table-list (list
                               (my-create-tags-if-needed
                                (concat (file-name-as-directory (getenv "WXWIN")) "include"))
                               (my-create-tags-if-needed "~/projs/Loris/loris/src/desktop")))
        )
       ((my-project-name-contains-substring "src/html")
        ;; html project donot need C++ tags
        (setq tags-table-list (list (my-create-tags-if-needed "~/projs/Loris/loris/src/html")))
        ))))

(add-hook 'after-save-hook 'my-auto-update-tags-when-save)
(add-hook 'js2-mode-hook 'my-setup-develop-environment)
(add-hook 'web-mode-hook 'my-setup-develop-environment)
(add-hook 'c++-mode-hook 'my-setup-develop-environment)
(add-hook 'c-mode-hook 'my-setup-develop-environment)