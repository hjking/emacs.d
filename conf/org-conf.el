
;; Filename: org-conf.el
;; Description: Setting for org.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2014-01-08 11:39:50

(message "%d: >>>>> Loading [ org ] Customization File ...." step_no)
(setq step_no (1+ step_no))

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

(setq org-directory "~/org")
;; (setq org-directory (concat my-emacs-dir "org"))
(setq org-default-notes-file (concat org-directory "/todo.org"))
(setq org-clock-persist-file (concat my-cache-dir "org-clock-save.el"))

(setq org-hide-leading-star t)
(setq org-startup-folded nil)  ;; open org in unfolded view
(setq org-edit-src-content-indentation 0)
(setq org-edit-timestamp-down-means-later t)

;; TODO Keywords
;; sequence: status keywords, can change from one to another
;; type: type keywords, can not switch betwwen each other
;; !: record time when state changed
;; @: need to leave some comments
(setq org-todo-keywords
    (quote ((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "MAYBE(m!)" "WAITING(w@/!)" "|" "HOLD(h@/!)" "DONE(x!)" "CANCELLED(c@/!)" "POSTPONED(p@/!)")
            (type "ACTION(a)" "ToBLOG(b)" "ARCHIVED(r)" "PHONE(p)" "MEETING(m)" "MEAL(e)" "|" "COMPLETED(x)")
            (type "REPORT" "BUG" "KNOWNCAUSE" "REVIEWED" "FEEDBACK" "|" "FIXED")
            (sequence "OPEN(O!)" "|" "CLOSED(C@/!)")
           )))

(setq org-todo-keyword-faces
      (quote (("TODO"      . (:foreground "red"          :weight bold))
              ("ToBLOG"    . (:foreground "red"          :weight bold))
              ("NEXT"      . (:foreground "orange"       :weight bold))
              ("STARTED"   . (:foreground "magenta"      :weight bold))
              ("DONE"      . (:foreground "forest green" :weight bold))
              ("WAITING"   . (:foreground "orange"       :weight bold))
              ("HOLD"      . (:foreground "magenta"      :weight bold))
              ("SOMEDAY"   . (:foreground "magenta"      :weight bold))
              ("CANCELLED" . (:foreground "forest green" :weight bold))
              ("MEETING"   . (:foreground "forest green" :weight bold))
              ("OPEN"      . (:foreground "red"          :weight bold))
              ("CLOSED"    . (:foreground "forest green" :weight bold))
              ("ARCHIVED"  . (:foreground "blue"         :weight bold))
              ("PHONE"     . (:foreground "forest green" :weight bold)))))

;; Fast todo selection allows changing from any task todo state to any other state
;; Changing a task state is done with C-c C-t KEY
(setq org-use-fast-todo-selection t)

;; Tags
;;  (@XXX) tags are mutually exclusive
;;    selecting one removes a similar tag already on the task
;;    These are the context tags
;;  other tags are not mutually exclusive and multiple tags
;;    can appear on a single task
;; Tags with fast selection keys
(setq org-tag-alist '((:startgroup nil)
                      ("@home" . ?h)
                      ("@errand" . ?e)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@office" . ?o)
                      ("@laptop" . ?l)
                      (:endgroup nil)
                      ("WEEKLY" . ?w)
                      ("HOMEWORK" . ?H)
                      ("PERSONAL" . ?P)
                      ("WORK" . ?W)
                      ("NOTE" . ?n)
                      ("CANCELLED" . ?c)
                      ("FLAGGED" . ??)
                      ("urgent" . ?u)
                      ("quantified" . ?q)))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

;; Org Agenda
;; (setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))
;; (setq org-agenda-files (quote ("~/emacs.d/org")))
;; (setq org-agenda-files '("~/org/widgets.org" "~/org/clients.org"))
;; (setq org-agenda-files (append org-agenda-files
;;      (list (expand-file-name (concat org-directory "/days")))))
(setq org-agenda-files (list (concat org-directory "/todo.org")
                             (concat org-directory "/habit.org")
                             (concat org-directory "/personal.org")
                             (concat org-directory "/work/fabric.org")
                             (concat org-directory "/work/misc.org")
                             (concat org-directory "/meeting.org")
                             (concat org-directory "/books.org")
                             (concat org-directory "/call.org")))
;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)
;; How many days should the default agenda show?
;; (setq org-agenda-ndays (* 6 7))  ;; six weeks
(setq org-agenda-ndays 'month)  ; a month
;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates nil)
;; see deadlines in the agenda view 7 days before the due date
(setq org-deadline-warning-days 7)
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-skip-scheduled-delay-if-deadline t)
;; the agenda start on Monday, or better today?
(setq org-agenda-start-on-weekday nil)
;; show entries from the Emacs diary
(setq org-agenda-include-diary t)
(setq org-agenda-diary-file (concat org-directory "/diary.org"))
;; any time strings in the heading are shown in the agenda
(setq org-agenda-insert-diary-extract-time t)
(setq org-agenda-span 2)
(setq org-agenda-show-log t)
;; (setq org-agenda-time-grid
;;       '((daily today require-timed)
;;        "----------------"
;;        (800 1000 1200 1400 1600 1800)))
;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))
;; Display tags farther right
(setq org-agenda-tags-column -102)

(setq org-columns-default-format "%30ITEM %15SCHEDULED %5TODO %5PRIORITY %Effort{:} %TAGS")
; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
;; (define-key org-agenda-mode-map "Y" 'org-agenda-todo-yesterday)
;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)
;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines t)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled t)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
;; (setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
;; (setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; org agenda custom commands
(setq org-agenda-custom-commands
           '(
             ("c" "Desk Work" tags-todo "computer|laptop"
                ((org-agenda-sorting-strategy '(priority-up effort-down))) ;; set local options
                ((concat org-directory "/computer.html"))) ;; export to file
             ;; overview of deadlines due within the next 60 days
             ("d" "Upcoming deadlines" agenda ""
               ((org-agenda-time-grid nil)
                (org-deadline-warning-days 60)        ;; [1] shows all deadlines that fall due within the upcoming year
                (org-agenda-entry-types '(:deadline))  ;; [2] looking for deadlines and nothing else so quite efficiently
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
             ("G" "GTD Block Agenda"
               ((tags-todo "office|work")
                (tags-todo "computer")
                (tags-todo "phone")
                (tags-todo "home")
                (tags-todo "errands"))
               nil                      ;; i.e., no local settings
               ((concat org-directory "/next-actions.html"))) ;; exports block to this file with C-c a e
             ;; block agenda views
             ("h" . "HOME+Name tags searches") ; "h" prefix
             ("ha" "Agenda and Home-related tasks"
               ((agenda "")
                (tags-todo "home")
                (tags "garden")))
             ("hc" tags "+home+child")
             ("hl" tags "+home+love")
             ("hp" tags "+home+parents")
             ;; display next 10 entries with a 'NEXT' TODO keyword.
             ("n" todo "NEXT"
               ((org-agenda-max-entries 10)))
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
             ;; run a tags/property search on files other than the agenda files
             ("r" "Reference material" tags ""
               ((org-agenda-files (file-expand-wildcards (concat org-directory "/ref/*.org")))))
             ("x" agenda)
             ("y" agenda*)
             ("w" "Weekly Review"
                ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
                                                ;; type "l" in the agenda to review logged items
                (stuck "") ;; review stuck projects as designated by org-stuck-projects
                (todo "MAYBE") ;; review maybe items
                (todo "SOMEDAY") ;; review someday items
                (todo "WAITING"))) ;; review waiting items
             ("W" todo-tree "WAITING") ; global search for TODO entries with 'WAITING' as the TODO keyword only in current buffer and displaying the result as a sparse tree
             ("u" tags "+boss-urgent") ; global tags search for headlines marked ':boss:' but not ':urgent:'
             ("v" tags-todo "+boss-urgent") ; global tags search for headlines marked ':boss:' but not ':urgent:', limiting the search to headlines that are also TODO items
             ("U" tags-tree "+boss-urgent") ; global tags search for headlines marked :boss: but not :urgent:', but only in the current buffer and displaying the result as a sparse tree
             ))

;; clock
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; automatically assign tags to tasks based on state changes
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING") ("HOLD"))
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
; Use full outline paths for refile targets
(setq org-refile-use-outline-path nil)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)
; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
;; (setq org-refile-targets
;;       '(((concat org-directory "/contacts.org") . (:maxlevel . 2))
;;         ((concat org-directory "/decisions.org") . (:maxlevel . 3))
;;         ((concat org-directory "/business.org") . (:maxlevel . 4))
;;         ((concat org-directory "/organizer.org") . (:maxlevel . 4))
;;         ((concat org-directory "/outline.org") . (:maxlevel . 3))))
(setq org-blank-before-new-entry nil)
; Exclude DONE state tasks from refile targets
(defun my/verify-refile-target ()
  "Exclude todo keywords with a DONE state from refile targets"
  (or (not (member (nth 2 (org-heading-components)) org-done-keywords)))
      (save-excursion (org-goto-first-child)))
(setq org-refile-target-verify-function 'my/verify-refile-target)

;; Strike through DONE headlines
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
;; (setq org-tags-column 80)

; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties
    (quote (("Effort_ALL" . "0:05 0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
            ("STYLE_ALL" . "habit"))))

;; To make org show leading stars use
(setq org-hide-leading-stars nil)
;; org-indent mode on by default at startup with the following setting:
(setq org-startup-indented t)
;; hides blank lines between headings which keeps folded view nice and compact.
(setq org-cycle-separator-lines 0)
;; prevents creating blank lines before headings but allows list items to adapt to existing blank lines around the items:
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))
;; Adding new tasks quickly without disturbing the current task content
(setq org-insert-heading-respect-content nil)

;;; Publishing
(setq my-org-publish-dir
      (expand-file-name "public_html" (directory-file-name
                                       (file-name-directory
                                        (directory-file-name org-directory)))))
(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)
(setq org-src-fontify-natively t)
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (sh . t)
   (perl . t)
   (python .t)
   ))
(setq org-publish-project-alist
      '(
        ("org-note"
          :base-directory "~/org/"
          :publishing-directory "~/org/public_html/"
          :base-extension "org"
          :recursive t
          :publishing-function org-html-export-to-html
          :auto-index nil
          :auto-sitemap t                  ; Generate sitemap.org automagically
          :index-filename "index.org"
          :index-title "index"
          :link-home "index.html"
          :headline-levels 4               ; Just the default for this project
          :section-numbers nil
          :export-creator-info nil    ; Disable the inclusion of "Created by Org" in the postamble
          :export-author-info nil     ; Disable the inclusion of "Author: Your Name" in the postamble
          :auto-postamble nil         ; Disable auto postamble
          ;; :style "<link rel=\"stylesheet\" href=\"./style/emacs.css\" type=\"text/css\"/>"
          :table-of-contents t)       ; Set this to "t" if you want a table of contents, set to "nil" disables TOC
        ("org-static"                ;Used to publish static files
         :base-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/org/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
        )
        ("org"
          :components ("org-notes" "org-static")) ;combine "org-static" and "org-static" into one function call
        ("blog-org"
          :base-directory "~/org/blog/org/"
          :publishing-directory "~/org/public_html/blog/source/"
          :base-extension "org"
          :recursive t
          :auto-index t
          :publishing-function org-html-export-to-html
          :headline-levels 4
          :html-extension "html"
          :creator-info nil
          :timestamp t
          ;; :auto-sitemap t ; Generate sitemap.org automagically
          :body-only t ;; Only export section between
          ;; :style "<link rel=\"stylesheet\" href=\"./style/emacs.css\" type=\"text/css\"/>"
          :table-of-contents t ;; export content
          )
        ("blog-static"
          :base-directory "~/org/blog/org/"
          :publishing-directory "~/org/public_html/blog/source/"
          :recursive t
          :base-extension "css\\|js\\|bmp\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el\\|pl\\|mht\\|log\\|bin_\\|bat\\|tst\\|doc\\|docx\\|gz"
          :publishing-function org-publish-attachment )
        ("blog"
         :components ("blog-org" "blog-static")
         :author "HJKing")
        ("org-ref"
          :base-directory "~/org/ref/"
          :base-extension "org"
          :publishing-directory "~/public_html/"
          :auto-sitemap t                  ; Generate sitemap.org automagically
          :recursive t
          :publishing-function org-html-export-to-html
          :headline-levels 4             ; Just the default for this project.
          :auto-preamble t
        )
        ))

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

; (add-hook 'org-mode-hook  ;; (1)
;  (lambda ()
;   (add-hook (make-local-variable 'after-save-hook) ;; (2)
;             'wicked/org-publish-files-maybe)))


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
(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

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

; Log
(setq org-log-done 'time) ;; mark DONE item with time
;; (setq org-log-done 'note) ;; leave some notes to DONE item
(setq org-log-into-drawer t)
(setq org-log-into-drawer "LOGBOOK")
(setq org-log-state-notes-insert-after-drawers nil)

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
              ("a" "Appointment" entry (file+headline (concat org-directory "/meeting.org") "Calendar")
                   "** APPT: %^{Description} %^g %?  Added: %U\n   SCHEDULED: %t")
              ("b" "Books to Read" entry (file+headline (concat org-directory "/books.org") "Books")
                   "** NEXT Read: %?\n   %i\n   %a")
              ("d" "Diary" entry (file+datetree (concat org-directory "/diary.org"))
                   "** %?\n  %U\n" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file (concat org-directory "/habit.org"))
                   "** TODO %?\n   %U\n   %a\n   SCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n   :PROPERTIES:\n   :STYLE: habit\n   :END:\n")
              ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
                   "** %^{Heading}\n  %U\n" :clock-in t :clock-resume t)
              ("l" "Log Time" entry (file+datetree (concat org-directory "/archive/log.org") )
                   ;; "** %U - %^{Activity}  :TIME:")
                   "** %U - %a  :TIME:")
              ("m" "Meeting" entry (file (concat org-directory "/meeting.org"))
                   "** MEETING with %? :MEETING:\n   SCHEDULED: %t\n  %U" :clock-in t :clock-resume t)
              ("n" "note" entry (file (concat org-directory "/notes.org"))
                   "** %? :NOTE:\n  %U\n  %a\n" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file (concat org-directory "/call.org"))
                   "** NEXT Phone Call: [[file:./contacts.org::*%i][%i]] %? :PHONE:\n   SCHEDULED: %t\n   %U" :clock-in t :clock-resume t)
              ("r" "Email Respond" entry (file (concat org-directory "/todo.org"))
                   "** NEXT Email Respond to %:from on %:subject\n  SCHEDULED: %t\n  %U\n  %a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("s" "Reference" entry (file+headline (concat org-directory "/ref/reference.org") "Reference")
                   "** %?\n  %i\n  %a")
              ("t" "todo" entry (file (concat org-directory "/todo.org"))
                   "** TODO %?\n   %U\n   %a\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file (concat org-directory "/refile.org"))
                   "** TODO Review %c\n   %U\n" :immediate-finish t)
            )))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))
(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;; archive place
(setq org-archive-location (concat org-directory "/archive/%s_archive::* Archived Tasks"))
(setq org-archive-mark-done nil)

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
(add-to-list 'org-agenda-custom-commands
             '("Qe" "Notes of everyday" agenda ""
               ((org-agenda-files (list (expand-file-name daypage-path))))))

(global-set-key "\C-con" 'todays-daypage)
(global-set-key "\C-coN" 'find-daypage)
;; }}}

(require 'org-checklist)

; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb
                          org-bibtex
                          org-crypt
                          org-gnus
                          org-id
                          org-info
                          org-jsinfo
                          org-habit
                          org-inlinetask
                          org-irc
                          org-mew
                          org-mhe
                          org-protocol
                          org-rmail
                          org-vm
                          org-wl
                          org-w3m)))

; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

;;; Priority
(setq org-highest-priority ?A)
(setq org-lowest-priority  ?E)
(setq org-default-priority ?E) ;; default #E
;; color
(setq org-priority-faces
  '((?A . (:background "red"        :foreground "white"     :weight bold))
    (?B . (:background "DarkOrange" :foreground "white"     :weight bold))
    (?C . (:background "yellow"     :foreground "DarkGreen" :weight bold))
    (?D . (:background "DodgerBlue" :foreground "black"     :weight bold))
    (?E . (:background "SkyBlue"    :foreground "black"     :weight bold))
))

;;; Dependence
;; if sub-item not DONE, item can not set to DONE
;; (setq org-enforce-todo-dependencies t)
;;
;; if the former item not DONE, the next can not set to DONE
;; use :PROPERTIES:
;;     :ORDERED: t
;;     :END:


;;; HTML5 Presentation export for Org-mode
;;; org-html5presentation.el
;; (require 'org-html5presentation)


(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'system-time-locale) "C")))

;;
(require 'org-jekyll-mode)
(setq org-jekyll/jekyll-project-root "~/org/blog/source/")
(setq org-jekyll/org-mode-project-root "~/org/blog/org/")
(setq org-jekyll/export-with-toc t)   ;; export content
