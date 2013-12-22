
;; Filename: org-conf.el
;; Description: Setting for org.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2012-06-11 15:21:46

(message "%d: >>>>> Loading [ org ] Customization File ...." step_no)
(setq step_no (1+ step_no))

(require 'org-install)
(require 'org-faces)
(require 'cus-edit)
(require 'org-publish)
(require 'org-agenda)
(require 'org-element)

(setq org-hide-leading-star t)
(setq org-startup-folded nil )  ;; open org in unfolded view

(setq org-publish-project-alist
      '(("note-org"
         :base-directory "~/emacs.d/org"
         :publishing-directory "~/emacs.d/org/publish"
         :base-extension "org"
         :recursive t
         :publishing-function org-publish-org-to-html
         :auto-index nil
         :index-filename "index.org"
         :index-title "index"
         :link-home "index.html"
         :section-numbers nil
         :style "<link rel=\"stylesheet\" href=\"./style/emacs.css\" type=\"text/css\"/>")
        ("note-static"
         :base-directory "~/emacs.d/org"
         :publishing-directory "~/emacs.d/org/publish"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
         :publishing-function org-publish-attachment)
        ("note"
         :components ("note-org" "note-static")
         :author "hon9jin@gmail.com"
         )))

(setq org-todo-keywords
           '((sequence
              "TODO(t)"
              "TOBLOG(b)"
              "NEXT(n)"
              "STARTED(s)"
              "|"
              "DONE(x)"
              "REVIEW"
              "CANCELLED(c@)"
              "SOMEDAY(s@/!)"
              "WAITING"
              "POSTPONED(p)"
              "ARCHIVED"
              "Action")
             (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
             (sequence "OPEN(O!)" "|" "CLOSED(C!)")
             ))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("TOBLOG" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("OPEN" :foreground "blue" :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold)
              ("ARCHIVED" . "blue")
              ("PHONE" :foreground "forest green" :weight bold))))

;; Fast todo selection allows changing from any task todo state to any other state
(setq org-use-fast-todo-selection t)

;; Tag tasks
(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("@errand" . ?e)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@office" . ?o)
                      ("@laptop" . ?l)
                      ("quantified" . ?q)))
; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Org Agenda
(setq org-agenda-files (file-expand-wildcards "~/emacs.d/org/*.org"))
(setq org-agenda-ndays (* 6 7))
(setq org-agenda-show-all-dates nil)
(setq org-deadline-warning-days 14)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 2)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")
; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
(define-key org-agenda-mode-map "Y" 'org-agenda-todo-yesterday)

;; clock
(setq org-log-done 'time) ;; mark DONE item with time
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; automatically assign tags to tasks based on state changes
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING"))
              ("STARTED" ("WAITING"))
              ("DONE" ("WAITING") ("CANCELLED")))))

;; Let org-mode use ido
(setq org-completion-use-ido t)
(setq org-remember-templates
    '(("Todo" ?t "* TODO %?\n %i\n %a" org-default-notes-file "Tasks")
      ("Idea" ?i "* %^{Title}\n %i\n %a" org-default-notes-file "Ideas")
      ("Journal" ?j "* %U %?\n\n %i\n %a" org-default-notes-file)))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-directory (concat my-emacs-dir "org"))
(setq org-default-notes-file (concat my-emacs-dir "org/todo.org"))

(setq org-list-indent-offset 2)


;; refilling
(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets
      '(("~/emacs.d/org/contacts.org" . (:maxlevel . 2))
        ("~/emacs.d/org/decisions.org" . (:maxlevel . 3))
        ("~/emacs.d/org/business.org" . (:maxlevel . 4))
        ("~/emacs.d/org/organizer.org" . (:maxlevel . 4))
        ("~/emacs.d/org/outline.org" . (:maxlevel . 3))))
(setq org-blank-before-new-entry nil)
(defun my/verify-refile-target ()
  "Exclude todo keywords with a DONE state from refile targets"
  (or (not (member (nth 2 (org-heading-components)) org-done-keywords)))
      (save-excursion (org-goto-first-child)))
(setq org-refile-target-verify-function 'my/verify-refile-target)

;; Strike through DONE headlines
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "LightSalmon" :strike-through t)))))

(setq org-tags-exclude-from-inheritance '("PROJECT"))

;; Enable filtering by effort estimates
(setq org-global-properties
      '(("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00")))


;; Publishing
(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-c t") 'goto-org-mode-todo-file)
(defun goto-org-mode-todo-file ()
  "Open the main todo file in `org-agenda-files'"
  (interactive)
  (find-file
   (let* ((time (decode-time))
          (hour (nth 2 time))
          (dow (nth 6 time))
          (is-weekend (or (= dow 0)
                          (= dow 6)))
          (is-work-hours (and (>= hour 8)
                              (<= hour 18))))
     (if (or is-weekend
             (not is-work-hours))
         (car org-agenda-files)
       (cadr org-agenda-files)))))

;;  (add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
;; Wrap long lines
(add-hook 'org-mode-hook 'toggle-truncate-lines)

;; flyspell mode for spell checking everything
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
;;    (add-hook 'org-mode-hook
;;              (lambda ()
;;                ;; yasnippet
;;                (make-variable-buffer-local 'yas/trigger-key)
;;                (org-set-local 'yas/trigger-key [tab])
;;                (define-key yas/keymap [tab] 'yas/next-field-group)
;;                ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files when directories are include
;;                ;; It expands the files in the directories individually
;;                (org-defkey org-mode-map "\C-c["    'undefined)
;;                (org-defkey org-mode-map "\C-c]"    'undefined)
;;                (local-set-key (kbd "C-c M-o") 'bh/mail-subtree)) 'append)
;;
;;    (defun bh/mail-subtree ()
;;      (interactive)
;;      (org-mark-subtree)
;;      (org-mime-subtree))

(defun sacha/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'sacha/yank-more)

; Structure templates
(setq org-structure-template-alist
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
        ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
        ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))

; Quick links
(setq org-link-abbrev-alist
  '(("google" . "http://www.google.com/search?q=")
    ("gmap" . "http://maps.google.com/maps?q=%s")
    ("blog" . "http://hjking.github.io")))

; Clocking
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)

; Speed commands
(setq org-use-effective-time t)
(add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
(add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
(add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))

; Attachments
(setq org-attach-store-link-p t)

;; speed up agenda overview
;; Blocked tasks are dimmed by default in the agenda
(setq org-agenda-dim-blocked-tasks nil)
(setq org-startup-folded t)
(setq org-agenda-inhibit-startup t)
;; inherited tags in todo/search/timeline/agenda
(setq org-agenda-use-tag-inheritance nil)
;; tags in todo agendas only
(setq org-agenda-use-tag-inheritance '(search timeline agenda))
