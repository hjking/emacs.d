
;; Filename: org-conf.el
;; Description: Setting for org.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2013-12-24 17:18:51

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(require 'org)
;; (require 'org-install)
;; (require 'org-faces)
;; (require 'cus-edit)
;; (require 'org-publish)
;; (require 'org-agenda)
;; (require 'org-element)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)

;; (setq org-directory "~/.emacs.d/org")
(setq org-directory (concat my-emacs-dir "org"))
(setq org-default-notes-file (concat org-directory "/todo.org"))

(setq org-hide-leading-star t)
(setq org-startup-folded nil)  ;; open org in unfolded view

;; TODO Keywords
(setq org-todo-keywords
    (quote ((sequence "TODO(t)" "NEXT(n)" "ACTION" "STARTED(s)" "|" "SOMEDAY(s@/!)" "MAYBE" "WAITING(w@/!)" "HOLD(h@/!)" "|" "DONE(x)" "CANCELLED(c@/!)" "POSTPONED(p)")
            (sequence "ToBLOG(b)" "ARCHIVED" "PHONE" "MEETING" "MEAL" "|" "COMPLETED")
            (sequence "REPORT" "BUG" "KNOWNCAUSE" "REVIEWED" "FEEDBACK" "|" "FIXED")
            (sequence "OPEN(O!)" "|" "CLOSED(C!)")
           )))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("ToBLOG" :foreground "red" :weight bold)
              ("NEXT" :foreground "orange" :weight bold)
              ("STARTED" :foreground "magenta" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold)
              ("OPEN" :foreground "red" :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold)
              ("ARCHIVED" . "blue")
              ("PHONE" :foreground "forest green" :weight bold))))

;; Fast todo selection allows changing from any task todo state to any other state
;; Changing a task state is done with C-c C-t KEY
(setq org-use-fast-todo-selection t)

;; Tags
(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("@errand" . ?e)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@office" . ?o)
                      ("@laptop" . ?l)
                      ("urgent" . ?u)
                      ("quantified" . ?q)))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Org Agenda
;; (setq org-agenda-files (file-expand-wildcards "~/emacs.d/org/*.org"))
(setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))
;; (setq org-agenda-files (quote ("~/emacs.d/org")))
;; How many days should the default agenda show?
(setq org-agenda-ndays (* 6 7))
(setq org-agenda-show-all-dates nil)
(setq org-deadline-warning-days 14)
;; the agenda start on Monday, or better today?
(setq org-agenda-start-on-weekday nil)
;; Should the agenda also show entries from the Emacs diary?
(setq org-agenda-include-diary t)
(setq org-agenda-span 2)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%30ITEM %15SCHEDULED %5TODO %5PRIORITY %Effort{:} %TAGS")
; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
;; (define-key org-agenda-mode-map "Y" 'org-agenda-todo-yesterday)
(setq org-agenda-repeating-timestamp-show-all t)
;; org agenda custom commands
(setq org-agenda-custom-commands
           '(
             ("c" "Desk Work" tags-todo "computer|laptop|phone"
                ((org-agenda-sorting-strategy '(priority-up effort-down))) ;; set local options
                ("~/org/computer.html")) ;; export to
             ("d" "Upcoming deadlines" agenda ""
               ((org-agenda-time-grid nil)
                (org-deadline-warning-days 365)        ;; [1]
                (org-agenda-entry-types '(:deadline))  ;; [2]
               ))
             ("f" occur-tree "\\<FIXME\\>") ; a sparse tree (again: current buffer only) with all entries containing the word FIXME
             ("g" . "GTD contexts")
             ("gc" "Computer" tags-todo "computer")
             ("ge" "Errands" tags-todo "errands")
             ("gh" "Home" tags-todo "home")
             ("go" "Office" tags-todo "office")
             ("gp" "Phone" tags-todo "phone")
             ;; a list of all tasks with the todo keyword STARTED
             ("gw" todo "STARTED")
             ;; block agenda views
             ("h" . "HOME+Name tags searches") ; "h" prefix
             ("ha" "Agenda and Home-related tasks"
               ((agenda "")
                 (tags-todo "home")
                 (tags "garden")))
             ("hl" tags "+home+love")
             ("hp" tags "+home+parents")
             ("hk" tags "+home+kimi")
             ("o" "Agenda and Office-related tasks"
               ((agenda "")
                 (tags-todo "work|errands")
                 (tags-todo "computer|office|phone"))
               ("~/org/next-actions.html"))
             ;; Priority
             ("p" . "Priorities")
             ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
             ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
             ("pc" "C items" tags-todo "+PRIORITY=\"C\"")
             ("Q" . "Custom queries of Archives and Publishments") ;; gives label to "Q"
             ("Qa" "Archive search" search ""
               ((org-agenda-files (file-expand-wildcards (concat org-directory "/archive/*.org")))))
             ("Qp" "Publish search" search ""
               ((org-agenda-files (file-expand-wildcards (concat org-directory "/publish/*.org")))))
             ("Qb" "Projects and Archive" search ""
               ((org-agenda-text-search-extra-files (file-expand-wildcards (concat org-directory "/archive/*.org")))))
                     ;; searches both projects and archive directories
             ("QA" "Archive tags search" org-tags-view ""
               ((org-agenda-files (file-expand-wildcards (concat org-directory "/archive/*.org")))))
             ("x" agenda)
             ("y" agenda*)
             ("w" todo "WAITING") ; global search for TODO entries with 'WAITING' as the TODO keyword
             ("W" todo-tree "WAITING") ; global search for TODO entries with 'WAITING' as the TODO keyword only in current buffer and displaying the result as a sparse tree
             ("u" tags "+boss-urgent") ; global tags search for headlines marked ':boss:' but not ':urgent:'
             ("v" tags-todo "+boss-urgent") ; global tags search for headlines marked ':boss:' but not ':urgent:', limiting the search to headlines that are also TODO items
             ("U" tags-tree "+boss-urgent") ; global tags search for headlines marked :boss: but not :urgent:', but only in the current buffer and displaying the result as a sparse tree
             ))

;; clock
(setq org-log-done 'time) ;; mark DONE item with time
;; (setq org-log-done 'note) ;; leave some notes to DONE item
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; automatically assign tags to tasks based on state changes
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("STARTED" ("WAITING"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Let org-mode use ido
(setq org-completion-use-ido t)
(setq org-remember-templates
    '(("Todo" ?t "* TODO %?\n %i\n %a" org-default-notes-file "Tasks")
      ("Idea" ?i "* %^{Title}\n %i\n %a" org-default-notes-file "Ideas")
      ("Journal" ?j "* %U %?\n\n %i\n %a" org-default-notes-file)))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-list-indent-offset 2)

;; Refiling means moving entries around
;; For example from a capturing location to the correct project
(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets
      '(((concat org-directory "/contacts.org") . (:maxlevel . 2))
        ((concat org-directory "/decisions.org") . (:maxlevel . 3))
        ((concat org-directory "/business.org") . (:maxlevel . 4))
        ((concat org-directory "/organizer.org") . (:maxlevel . 4))
        ((concat org-directory "/outline.org") . (:maxlevel . 3))))
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

;; To explicitly set only the strike-through attribute mentioned
(set-face-attribute 'org-done nil :strike-through t)
(set-face-attribute 'org-headline-done nil :strike-through t)

(setq org-tags-exclude-from-inheritance '("PROJECT"))

;; Enable filtering by effort estimates
(setq org-global-properties
      '(("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00")))


;; Publishing
(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)

(setq org-publish-project-alist
      '(("note-org"
         :base-directory org-directory
         :publishing-directory (concat org-directory "/publish")
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
         :base-directory org-directory
         :publishing-directory (concat org-directory "/publish")
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
         :publishing-function org-publish-attachment)
        ("note"
         :components ("note-org" "note-static")
         :author "hon9jin@gmail.com"
         )))

;; export an HTML version every time you save an Org file with keyword "#+PUBLISH"
(defun wicked/org-publish-files-maybe ()
  "Publish this file if it contains the #+PUBLISH: keyword"
  (save-excursion
   (save-restriction
    (widen)
    (goto-char (point-min))
    (when (re-search-forward
           "^#?[ \t]*\\+\\(PUBLISH\\)"
           nil t)
     (org-html-export-to-html)
     nil))))

(add-hook 'org-mode-hook  ;; (1)
 (lambda ()
  (add-hook (make-local-variable 'after-save-hook) ;; (2)
            'wicked/org-publish-files-maybe)))


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

;; Wrap long lines
;; (add-hook 'org-mode-hook 'toggle-truncate-lines)

;; flyspell mode for spell checking everything
;; (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

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

;; Capture
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (
              ("a" "Appointment" entry (file+headline (concat org-directory "/taskdiary.org") "Calendar")
                   "* APPT %^{Description} %^g %?  Added: %U")
              ("d" "Diary" entry (file+datetree (concat org-directory "/diary.org"))
                   "* %?\n  %U\n" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file (concat org-directory "/habit.org"))
                   "** TODO %?\n  %U\n  %a\n  SCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n  :PROPERTIES:\n  :STYLE: habit\n  :REPEAT_TO_STATE: NEXT\n  :END:\n")
              ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
                   "* %^{Heading}\n  %U\n" :clock-in t :clock-resume t)
              ("l" "Log Time" entry (file+datetree (concat org-directory "/log.org") )
                   "** %U - %^{Activity}  :TIME:")
              ("m" "Meeting" entry (file (concat org-directory "/meeting.org"))
                   "* MEETING with %? :MEETING:\n  %U" :clock-in t :clock-resume t)
              ("n" "note" entry (file (concat org-directory "/notes.org"))
                   "* %? :NOTE:\n  %U\n  %a\n" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file (concat org-directory "/call.org"))
                   "* Phone Call: %? :PHONE:\n  %U" :clock-in t :clock-resume t)
              ("r" "respond" entry (file (concat org-directory "/todo.org"))
                   "* NEXT Respond to %:from on %:subject\n  SCHEDULED: %t\n  %U\n  %a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("s" "Reference" entry (file+headline (concat org-directory "/ref/reference.org") "Reference")
                   "* %?\n  %i\n  %a")
              ("t" "todo" entry (file (concat org-directory "/todo.org"))
                   "* TODO %?\n  %U\n  %a\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file (concat org-directory "/refile.org"))
                   "* TODO Review %c\n  %U\n" :immediate-finish t)
            )))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; export an HTML version every time you save an Org file with keyword "#+PUBLISH"
(defun wicked/org-publish-files-maybe ()
  "Publish this file if it contains the #+PUBLISH: keyword"
  (save-excursion
   (save-restriction
    (widen)
    (goto-char (point-min))
    (when (re-search-forward
           "^#?[ \t]*\\+\\(PUBLISH\\)"
           nil t)
     (org-html-export-to-html)
     nil))))

(add-hook 'org-mode-hook  ;; (1)
 (lambda ()
  (add-hook (make-local-variable 'after-save-hook) ;; (2)
            'wicked/org-publish-files-maybe)))

;; archive place
(setq org-archive-location (concat org-directory "/archive/%s_archive::"))


;; get from http://almostobsolete.net/daypage.html
;; manage seperate day pages
;; {{{
(setq daypage-path (concat org-directory "/days/"))

(defun find-daypage (&optional date)
  "Go to the day page for the specified date,
   or toady's if none is specified."
  (interactive (list
                (org-read-date "" 'totime nil nil
                               (current-time) "")))
  (setq date (or date (current-time)))
  (find-file
       (expand-file-name
        (concat daypage-path
        (format-time-string "%Y-%m-%d" date) ".org"))))

(defun daypage-date ()
  "Return the date for the daypage visited by the current buffer
or nil if the current buffer isn't visiting a dayage"
  (let ((file (buffer-file-name))
        (root-path (expand-file-name daypage-path)))
    (if (and file
               (string= root-path (substring file 0 (length root-path)))
               (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\).org$" file))
        (flet ((d (i) (string-to-number (match-string i file))))
          (encode-time 0 0 0 (d 3) (d 2) (d 1)))
      nil)))

(defun daypage-next ()
  (interactive)
  (find-daypage
   (seconds-to-time (+ (time-to-seconds (daypage-date))
                       86400)))
  (run-hooks 'daypage-movement-hook))

(defun daypage-prev ()
  (interactive)
  (find-daypage
   (seconds-to-time (- (time-to-seconds (daypage-date))
                       86400)))
  (run-hooks 'daypage-movement-hook))

(defun daypage-next-week ()
  (interactive)
  (find-daypage
   (seconds-to-time (+ (time-to-seconds (daypage-date))
                       (* 86400 7))))
  (run-hooks 'daypage-movement-hook))

(defun daypage-prev-week ()
  (interactive)
  (find-daypage
   (seconds-to-time (- (time-to-seconds (daypage-date))
                       (* 86400 7))))
  (run-hooks 'daypage-movement-hook))

(defun todays-daypage ()
  "Go straight to todays day page without prompting for a date."
  (interactive)
  (find-daypage)
  (run-hooks 'daypage-movement-hook))

(defun todays-daypage ()
  "Go straight to today's day page without prompting for a date."
  (interactive)
  (find-daypage))

(defun yesterdays-daypage ()
  "Go straight to todays day page without prompting for a date."
  (interactive)
  (find-daypage
   (seconds-to-time (- (time-to-seconds (current-time))
                      86400)))
  (run-hooks 'daypage-movement-hook))

(defun daypage-new-item ()
  "Switches to the current daypage and inserts a top level heading and a timestamp"
  (interactive)
  (todays-daypage)
  (end-of-buffer)
  (if (not (bolp))
      (insert "\n"))
  (insert "* <" (format-time-string "%Y-%m-%d %a" (daypage-date)) "> "))

;; show them in agenda view
(setq org-agenda-files
  (append
    org-agenda-files
    (list (expand-file-name daypage-path))))

(global-set-key "\C-con" 'todays-daypage)
(global-set-key "\C-coN" 'find-daypage)
;; }}}

;;
