
(message "%d: >>>>> Loading [ Mode Line Customization ] ...." step_no)
(setq step_no (1+ step_no))
;; display time
(display-time-mode 1)
;; use 24-hour format
(setq display-time-24hr-format t)
(setq display-time-interval 10)
;; display time, day and date
(setq display-time-day-and-date t)
(display-time)
;; display line number in mode line
(setq line-number-mode t)

;; display column number in each mode line
(setq column-number-mode t)
; (column-number-mode 1)
(setq display-line-number-format "%3d ")
(setq line-number-display-limit 100000)
;; Show buffer size in mode-line
(size-indication-mode 1)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)

;; Display line number in mode-line
(defun get-lines-4-mode-line ()
  (let ((lines (count-lines (point-min) (point-max))))
    (concat (propertize
             (format "%d" lines)))))

(defun get-size-indication-format ()
  (if (and transient-mark-mode mark-active)
      (format "%dLs %dCs" (count-lines (region-beginning) (region-end)) (abs (- (mark t) (point))))
  ))

(when window-system
  (copy-face 'region 'region-invert)
  (invert-face 'region-invert))

(defun get-mode-line-region-face ()
  (if (and transient-mark-mode mark-active)
       (if (window-system 'region 'region-invert))))

(setq-default mode-line-position
    `((size-indication-mode
    ((:eval (propertize (get-size-indication-format)
                 'face (and transient-mark-mode mark-active (get-mode-line-region-face))
            ))))
))

;;  ;; Mode Line format
;;  (setq-default mode-line-format
;;        '("%e"
;;          "-"
;;          "%t"
;;          mode-line-mule-info
;;          "-"
;;          mode-line-client
;;          "-"
;;          mode-line-modified
;;          "-"
;;          mode-line-remote
;;          "--"
;;  ;;        mode-line-frame-identification
;;          mode-line-buffer-identification
;;          "-"
;;          vc-mode
;;          "-"
;;          mode-line-process
;;          "-%[("
;;          mode-name
;;  ;;        " "
;;          minor-mode-alist
;;          "%n"
;;          ")%]--"
;;          (-3 . "%p")  ;; position
;;          "--"
;;          (line-number-mode "L%l/")
;;          (:eval (get-lines-4-mode-line)) ;; total lines
;;          "--"
;;          (column-number-mode "C%c")
;;          ;; "--"
;;          ;; mode-line-position
;;          "--"
;;          "%I"
;;          "--"
;;          global-mode-string
;;          "-")
;;  )

;; (setq-default mode-line-format
;;       '(""
;;         mode-line-modified
;;         "[%b]"
;;         global-mode-string
;;         "%[("
;;         mode-name
;;         mode-line-process
;;         minor-mode-alist
;;         "%n" ")%]--"
;;         (line-number-mode "L%l--")
;;         (column-number-mode "C%c--")
;;         (-3 . "%p")  ;; position
;;         " -- "
;; ;;        user-login-name "@" system-name  ;; you@host.domain.org
;;         user-login-name "@" hostname  ;;  you@host
;;         ":"
;; ;;        "%f"  ;; print file with full path
;;         (:eval buffer-file-truename)  ;; print file with abbreviated path
;;         " %-"
;;         ) )

;; special format parameters
;   %b -- print buffer name.      %f -- print visited file name.
;   %F -- print frame name.
;   %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;         %& is like %*, but ignore read-only-ness.
;         % means buffer is read-only and * means it is modified.
;         For a modified read-only buffer, %* gives % and %+ gives *.
;   %s -- print process status.   %l -- print the current line number.
;   %c -- print the current column number (this makes editing slower).
;         To make the column number update correctly in all cases,
;         `column-number-mode' must be non-nil.
;   %i -- print the size of the buffer.
;   %I -- like %i, but use k, M, G, etc., to abbreviate.
;   %p -- print percent of buffer above top of window, or Top, Bot or All.
;   %P -- print percent of buffer above bottom of window, perhaps plus Top,
;         or print Bottom or All.
;   %n -- print Narrow if appropriate.
;   %t -- visited file is text or binary (if OS supports this distinction).
;   %z -- print mnemonics of keyboard, terminal, and buffer coding systems.
;   %Z -- like %z, but including the end-of-line format.
;   %e -- print error message about full memory.
;   %@ -- print @ or hyphen.  @ means that default-directory is on a
;         remote machine.
;   %[ -- print one [ for each recursive editing level.  %] similar.
;   %% -- print %.   %- -- print infinitely many dashes.
; Decimal digits after the % specify field width to which to pad.


;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(setq mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%02l" 'face 'font-lock-type-face) ","
      (propertize "%02c" 'face 'font-lock-type-face) 
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "] "


    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face 'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))  
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh"))))
    " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
    ))
