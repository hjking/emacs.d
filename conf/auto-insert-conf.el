
;; Filename: autoinsert-conf.el
;; Description: Setting for autoinsert.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2012-04-16 14:46:09
;;
(message "%d: >>>>> Loading [ auto insert ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'autoinsert)
(auto-insert-mode 1)  ;;; Adds hook to find-files-hook
(setq auto-insert 'other)
(setq auto-insert-query t) ;;; be prompted before insertion
; (setq auto-insert-directory "~/.emacs.d/templates/") ;;; Or use custom, *NOTE* Trailing slash important
(setq auto-insert-directory (concat my-emacs-dir "templates/")) ;;; Or use custom, *NOTE* Trailing slash important
;; (setq auto-insert-directory (concat (getenv "HOME") "/auto/"))
(add-hook 'find-file-hooks 'auto-insert)
;; auto-insert stuff
(define-auto-insert "\.py" "python.py")
(define-auto-insert "\.php" "php.php")
(define-auto-insert '("^build\\.xml\\'" . "Ant Compile Script") "build.xml")
(define-auto-insert '("\\.\\(v\\|vp\\)\\'" . "Verilog File") "verilog.v")
(define-auto-insert 'sh-mode '(nil "#!/bin/bash\n\n"))
(setq auto-insert-alist
  '(
    ("\\.cpp$" . ["cpp.cpp" auto-update-c-source-file])
    ("\\.h$"   . ["h.h" auto-update-header-file])
    ("\\.c$" . ["c.c" auto-update-c-source-file])
    ))
;; function replaces the string '@@@' by the current file
;; name. You could use a similar approach to insert name and date into
;; your file.
(defun auto-update-header-file ()
 (save-excursion
   (while (search-forward "@@@" nil t)
     (save-restriction
       (narrow-to-region (match-beginning 0) (match-end 0))
       (replace-match (upcase (file-name-nondirectory buffer-file-name)))
       (subst-char-in-region (point-min) (point-max) ?. ?_)
       ))
   )
 )

(defun my-insert-today ()
 "Insert today's date into buffer"
 (interactive)
 (insert (format-time-string "%A, %B %e %Y" (current-time))))

(defun auto-update-c-source-file ()
  (save-excursion
    ;; Replace HHHH with file name sans suffix
    (while (search-forward "HHHH" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (concat (file-name-sans-extension (file-name-nondirectory buffer-file-name)) ".h") t
               )
        ))
  )
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "@@@" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-nondirectory buffer-file-name))
        ))
  )
  (save-excursion
    ;; replace DDDD with today's date
    (while (search-forward "DDDD" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match "")
        (insert-today)
        ))
  )
)

(defun hjking/auto-replace-header-name ()
  (save-excursion
    (while (search-forward "###" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (upcase (file-name-nondirectory buffer-file-name)))
        (subst-char-in-region (point-min) (point-max) ?. ?_)
        (subst-char-in-region (point-min) (point-max) ?- ?_)
        ))
    )
  )

(defun hjking/auto-replace-file-name ()
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "(>>FILE<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-nondirectory buffer-file-name) t)
        ))
    )
  )

(defun hjking/auto-replace-date-time ()
  (save-excursion
    ;; replace DDDD with today's date
    (while (search-forward "(>>DATE<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match "" t)
        (my-insert-today)
        ))))

(defun hjking/auto-replace-file-name-no-ext ()
  (save-excursion
    ;; Replace @@@ with file name
    (while (search-forward "(>>FILE_NO_EXT<<)" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (file-name-sans-extension (file-name-nondirectory buffer-file-name)) t)
        ))
    )
  )

(defun hjking/auto-update-defaults ()
  (hjking/auto-replace-file-name)
  (hjking/auto-replace-file-name-no-ext)
  (hjking/auto-replace-date-time)
  )

(defun hjking/auto-update-header-file ()
  (hjking/auto-replace-header-name)
  (hjking/auto-replace-file-name)
  )

(defun hjking/auto-update-defaults ()
  (hjking/auto-replace-file-name)
  (hjking/auto-replace-file-name-no-ext)
  (hjking/auto-replace-date-time)
  )

