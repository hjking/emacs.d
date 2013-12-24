
;; The first line in this custom command contains:

;; 1. the key to trigger the search ("c")
;; 2. a description of the search for the agenda dispatcher ("Desk Work")
;; 3. the type of search (todo items matching a tag)
;; 4. the tag to search for ("computer")
;; 5. set local options for this agenda command only. Any options you set here will not interfere with your global agenda settings. In this particular command, the files widgets.org and clients.org will be searched and the agenda display will sort the todos first by priority (highest priority last), then by estimated effort (highest effort first)—regardless or your global org-agenda-sorting-strategy.
;; 6. specifies that this agenda view should be exported as HTML to "~/computer.html" when the agenda export function is called (C-c a e).


(setq org-agenda-custom-commands
      '(("O" "Office block agenda"
         ((agenda "" ((org-agenda-ndays 1)))
                      ;; limits the agenda display to a single day
          (tags-todo "+PRIORITY=\"A\"")
          (tags-todo "computer|office|phone")
          (tags "project+CATEGORY=\"elephants\"")
          (tags "review" ((org-agenda-files '("~/org/circuspeanuts.org"))))
                          ;; limits the tag search to the file circuspeanuts.org
          (todo "WAITING"))
         ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block
        ;; ...other commands here
        ))
;; As part of your typical routine, you'd like to see:
;; 1. your scheduled tasks and upcoming deadlines
;; 2. any urgent tasks with the priority "A"
;; 3. any tasks you can do at your computer or in your office or on the phone
;; 4. any headlines tagged "project" for which CATEGORY="elephants"
;; 5. any headlines tagged "review" in the file "circuspeanuts.org"
;; 6. any items that have the todo keyword "WAITING"


;; create block views for the weekly review
(setq org-agenda-custom-commands
      '(("W" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
                                           ;; type "l" in the agenda to review logged items
          (stuck "") ;; review stuck projects as designated by org-stuck-projects
          (todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
          (todo "MAYBE") ;; review someday/maybe items
          (todo "WAITING"))) ;; review waiting items
         ;; ...other commands here
        ))

;; "~/org/" for your project files
;; "~/website/" for your published files
;; "~/archive/" for old projects and notes
;; only want to search your project files
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
;; But there are no doubt times when you need to search your website or your archive files. To make this possible, you could create the following commands:
;; Now you are only a key command away from searching different sets of org files.
(setq org-agenda-custom-commands
      '(("Q" . "Custom queries") ;; gives label to "Q"
        ("Qa" "Archive search" search ""
         ((org-agenda-files (file-expand-wildcards "~/archive/*.org"))))
        ("Qw" "Website search" search ""
         ((org-agenda-files (file-expand-wildcards "~/website/*.org"))))
        ("Qb" "Projects and Archive" search ""
         ((org-agenda-text-search-extra-files (file-expand-wildcards "~/archive/*.org"))))
                ;; searches both projects and archive directories
        ("QA" "Archive tags search" org-tags-view ""
         ((org-agenda-files (file-expand-wildcards "~/archive/*.org"))))
        ;; ...other commands here
         ))

("w" todo "WAITING"
              ((org-agenda-sorting-strategy '(priority-down))
               (org-agenda-prefix-format "  Mixed: ")))

;; tweak display options for sparse tree searches via the variables org-show-hierarchy-above and org-show-entry-below
;; create custom agenda commands that default to a column view
(setq org-agenda-custom-commands
      '(("x" "With deadline columns" alltodo ""
         ((org-agenda-overriding-columns-format "%20ITEM %DEADLINE")
          (org-agenda-view-columns-initially t)))))

;; an example of a weekly calendar using org-agenda-entry-types.
(setq org-agenda-custom-commands
      '(("c" "Calendar" agenda ""
         ((org-agenda-ndays 7)                          ;; [1] create a weekly calendar
          (org-agenda-start-on-weekday 0)               ;; [2] starts on Sunday
          (org-agenda-time-grid nil)
          (org-agenda-repeating-timestamp-show-all t)   ;; [3] includes all instances of repeating timestamps
          (org-agenda-entry-types '(:timestamp :sexp))))  ;; [4] only searches for timestamps and diary sexps and does not bother with deadlines and scheduling timestamps
      ;; other commands go here
        ))
;; org-agenda-entry-types is a list that consists of any or all of the following items:
;; :timestamp
;; List items containing a date stamp or date range matching the selected date. This includes sexp entries in angular brackets.
;; :sexp
;; List entries resulting from plain diary-like sexps.
;; :deadline
;; List deadline due on that date. When the date is today, also list any deadlines past due, or due within `org-deadline-warning-days'. `:deadline' must appear before `:scheduled' if the setting of `org-agenda-skip-scheduled-if-deadline-is-shown' is to have any effect.
;; :scheduled
;; List all items which are scheduled for the given date. The diary for today also contains items which were scheduled earlier and are not yet marked DONE.
;; if you want to exclude all agenda entry types, just set org-agenda-entry-types to nil


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an overview of all deadlines due within the next 365 days
(setq org-agenda-custom-commands
      '(("d" "Upcoming deadlines" agenda ""
                ((org-agenda-time-grid nil)
                 (org-deadline-warning-days 365)        ;; [1] shows all deadlines that fall due within the upcoming year
                 (org-agenda-entry-types '(:deadline))  ;; [2] looking for deadlines and nothing else so quite efficiently
                 ))
      ;; other commands go here
       ))

;; Keyboard shortcuts to search by GTD contexts.
(setq org-agenda-custom-commands
      '(("g" . "GTD contexts")
        ("go" "Office" tags-todo "office")
        ("gc" "Computer" tags-todo "computer")
        ("gp" "Phone" tags-todo "phone")
        ("gh" "Home" tags-todo "home")
        ("ge" "Errands" tags-todo "errands")
        ("G" "GTD Block Agenda"
         ((tags-todo "office")
          (tags-todo "computer")
          (tags-todo "phone")
          (tags-todo "home")
          (tags-todo "errands"))
         nil                      ;; i.e., no local settings
         ("~/next-actions.html")) ;; exports block to this file with C-c a e
       ;; ..other commands here
        ))

;; Shortcuts to display tasks by priority level:
(setq org-agenda-custom-commands
      '(("p" . "Priorities")
        ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
        ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
        ("pc" "C items" tags-todo "+PRIORITY=\"C\"")
        ;; ...other commands here
        ))

;; see a weekly planner containing only appointments—that is, with deadlines and scheduled items omitted
(setq org-agenda-custom-commands
'(("c" "Weekly schedule" agenda ""
         ((org-agenda-ndays 7)          ;; agenda will start in week view
          (org-agenda-repeating-timestamp-show-all t)   ;; ensures that repeating events appear on all relevant dates
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
        ;; limits agenda view to timestamped items
        ;; ...other commands here
        ))

;;  deadlines due within the next 60 days
(setq org-agenda-custom-commands
      '(("d" "Upcoming deadlines" agenda ""
         ((org-agenda-entry-types '(:deadline))
          ;; a slower way to do the same thing
          ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
          (org-agenda-ndays 1)
          (org-deadline-warning-days 60)
          (org-agenda-time-grid nil)))
        ;; ...other commands here
        ))

;; a black-and-white block agenda that lists:
;; An overview of appointments for the next seven days.
;; 	timestamped items but not deadlines or scheduled tasks
;; A compact daily agenda.
;; A list of todos sorted by context.
(setq org-agenda-custom-commands
      '(("P" "Printed agenda"
         ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                      (org-agenda-start-on-weekday nil)         ;; calendar begins today
                      (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-entry-types '(:timestamp :sexp))))
          (agenda "" ((org-agenda-ndays 1)                      ;; daily agenda
                      (org-deadline-warning-days 7)             ;; 7 day advanced warning for deadlines
                      (org-agenda-todo-keyword-format "[ ]")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))
          (todo "TODO"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
         ((org-agenda-with-colors nil)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
           (ps-landscape-mode t))
         ("~/agenda.ps"))
        ;; other commands go here
        ))

;;
(setq org-agenda-custom-commands
      '(("a" "My custom agenda"
	 ((org-agenda-list nil nil 1)
	  (tags "PROJECT-WAITING")
	  (tags-todo "WAITING")
	  (tags-todo "-MAYBE")))))


;; display the next five entries with a 'NEXT' TODO keyword.
(setq org-agenda-custom-commands
      '(("n" todo "NEXT"
         ((org-agenda-max-entries 5)))))

;;
(add-to-list 'org-agenda-custom-commands
             '("B" "Big books (fast)" search "{:BIB_PAGES:\\s-+[0-9]\\{4\\}}"))

;; perform the "urgent wedding tasks" search
(add-to-list 'org-agenda-custom-commands
             '("w" "Getting married next week!" search "!+wedding +{:\\(urgent\\|important\\):}"))

;; run a tags/property search on files other than the agenda files
(add-to-list 'org-agenda-custom-commands
             '("r" "Reference material" tags ""
               ((org-agenda-files (file-expand-wildcards "~/ref/*.org")))))

;;
