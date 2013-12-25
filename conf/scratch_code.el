
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



;; see a weekly planner containing only appointments—that is, with deadlines and scheduled items omitted
(setq org-agenda-custom-commands
'(("c" "Weekly schedule" agenda ""
         ((org-agenda-ndays 7)          ;; agenda will start in week view
          (org-agenda-repeating-timestamp-show-all t)   ;; ensures that repeating events appear on all relevant dates
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
        ;; limits agenda view to timestamped items
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
