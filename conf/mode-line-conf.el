
(message "%d: >>>>> Loading [ Mode Line Customization ] ...." step_no)
(setq step_no (1+ step_no))

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
;   Decimal digits after the % specify field width to which to pad.
;   mode-line-mule-info: displays information about the language environment, buffer coding system, and current input method
;   mode-line-modified: displays whether the current buffer is modified
;   mode-line-frame-identification: identifies the current frame
;   mode-line-buffer-identification: identifies the buffer
;   mode-name: name of the current buffer's major mode
;   minor-mode-alist: minor mode list
;   mode-line-process: information on process status in modes used for communicating with subprocesses
;   vc-mode: whether the buffer's visited file is maintained with version control

; Default: ("%e" mode-line-front-space mode-line-mule-info mode-line-client
;           mode-line-modified mode-line-remote mode-line-frame-identification
;           mode-line-buffer-identification "   " mode-line-position (vc-mode vc-mode)
;           " " mode-line-modes mode-line-misc-info mode-line-end-spaces)

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
                 ))))))

;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
;; Format: | filename (line, col) [pos/size] [major mode] [overwrite modified] [time] [minor mode] |
; (setq mode-line-format
;   (list
;     ;; the buffer name; the file name as a tool tip
;     '(:eval (propertize "%b " 'face 'font-lock-keyword-face
;         'help-echo (buffer-file-name)))

;     ;; line and column
;     "(" ;; '%02' to set to 2 chars at least; prevents flickering
;       (propertize "%02l" 'face 'font-lock-type-face) ","
;       (propertize "%02c" 'face 'font-lock-type-face)
;     ") "

;     ;; relative position, size of file
;     "["
;     (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
;     "/"
;     (propertize "%I" 'face 'font-lock-constant-face) ;; size
;     "] "

;     ;; the current major mode for the buffer.
;     "["

;     '(:eval (propertize "%m" 'face 'font-lock-string-face
;               'help-echo buffer-file-coding-system))
;     "] "


;     "[" ;; insert vs overwrite mode, input-method in a tooltip
;     '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;               'face 'font-lock-preprocessor-face
;               'help-echo (concat "Buffer is in "
;                            (if overwrite-mode "overwrite" "insert") " mode")))

;     ;; was this buffer modified since the last save?
;     '(:eval (when (buffer-modified-p)
;               (concat ","  (propertize "Mod"
;                              'face 'font-lock-warning-face
;                              'help-echo "Buffer has been modified"))))

;     ;; is this buffer read-only?
;     '(:eval (when buffer-read-only
;               (concat ","  (propertize "RO"
;                              'face 'font-lock-type-face
;                              'help-echo "Buffer is read-only"))))
;     "] "

;     ;; add the time, with the date and the emacs uptime in the tooltip
;     '(:eval (propertize (format-time-string "%H:%M")
;               'help-echo
;               (concat (format-time-string "%c; ")
;                       (emacs-uptime "Uptime:%hh"))))
;     " -- ["
;     ;; i don't want to see minor-modes; but if you want, uncomment this:
;     minor-mode-alist  ;; list of minor modes
;     "] %-" ;; fill with '-'
;     ))

; (set-face-background 'modeline "#4477aa")

;; From http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;;
;; Mode line setup
; (setq-default mode-line-format
;  '(; Position, including warning for 80 columns
;    (:propertize "%4l:" face mode-line-position-face)
;    (:eval (propertize "%3c" 'face
;                       (if (>= (current-column) 80)
;                           'mode-line-80col-face
;                         'mode-line-position-face)))
;    ; emacsclient [default -- keep?]
;    mode-line-client
;    "  "
;    ; read-only or modified status
;    ; buffer is modified, use a bright white and red block
;    ; read-only, use a blue indicator
;    (:eval
;     (cond (buffer-read-only
;            (propertize " RO " 'face 'mode-line-read-only-face))
;           ((buffer-modified-p)
;            (propertize " ** " 'face 'mode-line-modified-face))
;           (t "      ")))
;    "    "
;    ; directory and buffer/file name
;    (:propertize (:eval (shorten-directory default-directory 30))
;                 face mode-line-folder-face)
;    (:propertize "%b"
;                 face mode-line-filename-face)
;    ; narrow [default -- keep?]
;    " %n "
;    ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
;    (vc-mode vc-mode)
;    "  %["
;    (:propertize mode-name
;                 face mode-line-mode-face)
;    "%] "
;    (:eval (propertize (format-mode-line minor-mode-alist)
;                       'face 'mode-line-minor-mode-face))
;    (:propertize mode-line-process
;                 face mode-line-process-face)
;    (global-mode-string global-mode-string)
;    "    "
;    ; nyan-mode uses nyan cat as an alternative to %p
;    (:eval (when nyan-mode (list (nyan-create))))
;    ))



(setq-default mode-line-format
 '(
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; directory and buffer/file name
    (:propertize (:eval (shorten-directory default-directory 20))
                 face mode-line-folder-face)
    (:propertize "%b"
                 face mode-line-filename-face)
    ; narrow [default -- keep?]
    " %n "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    "("
    (:propertize "%3l/" face mode-line-position-face)
    (:eval (get-lines-4-mode-line)) ;; total lines
    ":"
    (:eval (propertize "%2c" 'face
                       (if (>= (current-column) 80)
                           'mode-line-80col-face
                         'mode-line-position-face)))
    ") "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; relative position, size of file
    "["
    ; (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    (-3 . "%p")  ;; position
    "/"
    ; (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "%I"
    "] "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; read-only or modified status
    ; buffer is modified, use a bright white and red block
    ; read-only, use a blue indicator
    (:eval
     (cond (buffer-read-only
            (propertize " RO " 'face 'mode-line-read-only-face))
           ((buffer-modified-p)
            (propertize " ** " 'face 'mode-line-modified-face))
           (t "      ")))
    "  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; emacsclient [default -- keep?]
    mode-line-client
    "  "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
    (vc-mode vc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Major mode
    ; "["
    ; '(:eval (propertize "%m" 'face 'font-lock-string-face
    ;           'help-echo buffer-file-coding-system))
    ; "] "
    " %["
    (:propertize mode-name
                 face mode-line-mode-face)
    "%] "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    "("
    (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
    ") "
    (:propertize mode-line-process
                 face mode-line-process-face)
    mode-line-misc-info
    ; (global-mode-string global-mode-string)
    ; "  --"
    ; '(which-func-mode ("" which-func-format "--"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; nyan-mode uses nyan cat as an alternative to %p
    ;(:eval (when nyan-mode (list (nyan-create))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ))


;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

; (set-face-attribute 'mode-line nil
;     :foreground "gray60" :background "gray20"
;     :inverse-video nil
;     :box '(:line-width 6 :color "gray20" :style nil))
; (set-face-attribute 'mode-line-inactive nil
;     :foreground "gray80" :background "gray40"
;     :inverse-video nil
;     :box '(:line-width 6 :color "gray40" :style nil))

;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
; (set-face-background 'mode-line          "#4466aa")
; (set-face-background 'mode-line-inactive "#99aaff")
;; a box around the active mode-line
; (custom-set-faces
;    '(mode-line ((t (:box (:line-width 2 :color "red"))))))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo")
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "green")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40")
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

(provide 'mode-line-conf)