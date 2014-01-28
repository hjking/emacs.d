
(message "%d: >>>>> Loading [ Functions ] Customizations ...." step_no)
(setq step_no (1+ step_no))
;; (defvar user-emacs-directory (or user-emacs-directory "~/.emacs.d/"))
(defcustom *conf-root* (concat my-emacs-dir "conf/")
  "Location of configuration files to be loaded at startups")

(defcustom *plugin* (concat my-emacs-dir  "plugin/")
  "Location of third-party files to be loaded at startup")

;; Load all my plugins
(defun load-paths()
  "Loads plugins from the path specified in *plugin*"
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (let* ((my-lisp-dir *plugin*)
             (default-directory my-lisp-dir))
        (setq load-path (cons my-lisp-dir load-path))
        (normal-top-level-add-subdirs-to-load-path)))
  (setq load-path (cons *conf-root* load-path)))

;; Loads configuration files.
(defun load-config()
  "Loads configuration files from the path specified in *conf-root*"
  (let ((*lst* (directory-files *conf-root* nil ".el$")))
    (mapc #'(lambda(x)
              (load-library (concat (subseq x 0 (- (length x) 3)) ""))) *lst*)))

(defvar missing-packages-list nil
  "List of packages that try-require can not find.")

(defun prepend-path ( my-path )
    (setq load-path (cons (expand-file-name my-path) load-path)))

(defun append-path ( my-path )
    (setq load-path (append load-path (list (expand-file-name my-path)))))

;; attempt to load a feature/library, failing silently
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
    (progn
      (message "    Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "    Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
      (progn
        (message "    Checking for library `%s'... Missing" feature)
        (add-to-list 'missing-packages-list feature 'append))
      nil)
  )
)

;;;###autoload
(defun am-add-hooks (hooks function &optional append local)
  "Call `add-hook' on hook list HOOKS use arguments FUNCTION, APPEND, LOCAL.
HOOKS can be one list or just a hook."
  (if (listp hooks)
      (mapc
       `(lambda (hook)
          (add-hook hook ',function append local))
       hooks)
    (add-hook hooks function append local)))

;;;###autoload
(defun am-intern (&rest strings)
  "`intern' use STRINGS."
  (intern
   (apply
    'concat
    (mapcar
     (lambda (element)
       (if (stringp element) element (symbol-name element)))
     strings))))

;;;###autoload
(defun am-variable-is-t (symbol)
  "Return SYMBOL's value is t or not."
  (and (boundp symbol) (symbol-value symbol)))

;;;###autoload
(defmacro am-def-active-fun (symbol &optional fun-name)
  "Make definition of function judge variable is active or not."
  `(defun ,(if fun-name fun-name symbol) ()
     ,(concat "`" (symbol-name symbol) "' is t or not.")
     (am-variable-is-t ',symbol)))

;;;###autoload
(defun am-forward-word-or-to-word ()
  "`forward-word' or `forward-to-word'.
If after excute `forward-to-word', current position
is at next line, then rollback and excute `forward-word'"
  (interactive)
  (let ((noo (line-number-at-pos)) no)
    (save-excursion
      (forward-to-word 1)
      (setq no (line-number-at-pos)))
    (if (> no noo)
        (forward-word)
      (forward-to-word 1))))

;;;###autoload
(defmacro am-with-temp-mode (mode &rest body)
  "Create a temporary buffer with mode MODE, and evaluate BODY there like `progn'.
See also `with-temp-buffer'."
  `(with-temp-buffer
     (funcall ,mode)
     ,@body))

;;;###autoload
(defun am-equal-ignore-case (str1 str2)
  "STR1 equal ignore case to STR2 or not."
  (string= (downcase str1) (downcase str2)))

;; open my Emacs init file
(defun my-open-dot-emacs ()
  "Opening `~/.emacs'."
  (interactive)
  (find-file "~/.emacs")
)

;;; === insert filename ===
(defun my-insert-file-name ()
  "Insert the buffer-file-name at point."
  (interactive)
  (insert buffer-file-name)
)

;;; === insert date ===
(defun my-insert-date-stamp ()
  "Insert a time stamp at point."
  (interactive)
;;  (shell-command "date +'%Y-%B-%d' | tr -d '\n' " (quote (4)) nil) )
  (insert (format-time-string "%Y-%m-%d" (current-time)))
)

;;; === insert date and time ===
(defun my-insert-date-time-stamp ()
  "Insert date and time at point."
  (interactive)
;;  (shell-command "date +'%Y-%B-%d (%H:%M)' | tr -d '\n' " (quote (4)) nil) )
  (insert (format-time-string "%Y-%m-%d %3a %H:%M:%S" (current-time)))
)

(defun my-insert-date (prefix)
  "Insert the current date in ISO format. With prefix-argument,
  add day of week. With two prefix arguments, add day of week and
  time."
  (interactive "P")
  (let ((format (cond ((not prefix) "%Y-%m-%d")
                      ((equal prefix '(4)) "%Y-%m-%d %a")
                      ((equal prefix '(16)) "%Y-%m-%d %a %H:%M"))))
    (insert (format-time-string format))
   )
)

;;; === insert-braces ===
(defun my-insert-braces ()
  "Insert matched braces, leave point inside."
  (interactive "*")
  (let (blink-paren-function) ;nil it temporarily
    (execute-kbd-macro
      (if (and (eq major-mode 'cc-c++-mode) (not (looking-at ";")))
        "{};" "{}"
      )
    )
  )
  (backward-sexp 1)
  (if
    (save-excursion
        (forward-char -1)
        (looking-at "\\$")
    )
    nil
    (reindent-then-newline-and-indent)
;;    (c-indent-exp)
    (forward-char 1)
    (newline-and-indent)
  )
)

;;;; Function Set for Copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; === copy line ===
(defun my-copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line"
  (interactive "P")
  (let ((beg (line-beginning-position))
     (end (line-end-position arg)))
  (copy-region-as-kill beg end))
)

(defun ew-copy-lines (arg)
      "Copy lines (as many as prefix argument) in the kill ring"
      (interactive "p")
      (kill-ring-save (line-beginning-position)
                      (line-beginning-position (+ 1 arg)))
      (message "%d line%s copied" arg (if (= 1 arg) "" "s")))


;;; === copy word ===
(defun my-copy-word (&optional arg)
  "Copy word at point"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point)))
     (end (progn (forward-word arg) (point))))
  (copy-region-as-kill beg end))
)

;;; === copy region ===
(defun my-copy-paragraph (&optional arg)
  "Copy paragraphe at point"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point)))
     (end (progn (forward-paragraph arg) (point))))
  (copy-region-as-kill beg end))
)

(defun copy-file-path ()
  "Copy the current buffer's file path or dired path to kill-ring."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (kill-new default-directory)
    (kill-new (buffer-file-name))
    )
  (message "** File path copied.")
)

;;; === duplicate current line===
(defun my-duplicate-line ()
  "Duplicate current line."
  (interactive)
  (progn
    (my-kill-ring-save-line) ; save line
    (save-excursion ; duplicate line
      (end-of-line)
      (insert "\n")
      (yank)
    )
    (let ( (n (my-get-col)) ) ; move to new line, goto same column
      (forward-line +1)
      (move-to-column n)
    )
  )
)

;;; === delete current line ===
(defun my-delete-line ()
  "Delete current line."
  (interactive)
  (progn
    (beginning-of-line) (kill-line 1)
  )
)

;;; === delete ^M ===
(defun my-delete-crtl-M ()
  "Delete all ^M (dos --> unix line endings)."
  (interactive)
  (progn
    (save-excursion
      (goto-line 1) (replace-regexp "+" "")
    )))

;; convert a buffer from DOS `^M' end of lines to Unix end of lines
(defun dos-to-unix ()
  "Remove all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))
  )
)

;; convert a buffer from Unix end of lines to DOS `^M' end of lines
(defun unix-to-dos ()
  "Convert a buffer from unix to dos."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))
   )
)

(defun dos2unix ()
  "dos2unix on current buffer."
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun unix2dos ()
  "unix2dos on current buffer."
  (interactive)
  (set-buffer-file-coding-system 'dos))

;; Behave like vi's o command
(defun open-newline-below (arg)
  "Move to the next line and then opens a line.
  See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun open-newline-above (arg)
  "Open a new line before the current one.
   See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;;; === move line ===
;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing Alt-Up (or Down)
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col)
    )
  )
)

(defun move-line-down ()
	"Swap the current line with the line below."
    (interactive)
    (let ((col (current-column)))
         (save-excursion
           (forward-line)
           (transpose-lines 1))
         (forward-line)
         (move-to-column col)))

(defun move-line-up ()
	"Swap the current line with the line above."
    (interactive)
    (let ((col (current-column)))
         (save-excursion
            (forward-line)
            (transpose-lines -1))
         (move-to-column col)))

(defun move-nline-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-nline-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun swap-line-up ()
  "Swap the current line with the line above."
  (interactive)
  (transpose-lines 1)
  (beginning-of-line -1))

(defun swap-line-down ()
  "Swap current line with the line below."
  (interactive)
  (beginning-of-line 2) (transpose-lines 1) (beginning-of-line 0))

;;; === match paren ===
(defun my-match-paren ()
  "Move to the parenthesis matching the one under the cursor."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;;;###autoload
;;;use % to jumo to the matching parenthesis
(defun goto-match-paren (arg)
  "Go to the matching paren if on a paren, otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

(defun match-parenthesis (arg)
   "Match the current character according to the syntax table.
   Based on the freely available match-paren.el by Kayvan Sylvan.
   I merged code from goto-matching-paren-or-insert and match-it.
   You can define new \"parentheses\" (matching pairs).
   Example: angle brackets. Add the following to your .emacs file:
      (modify-syntax-entry ?< \"(>\" )
      (modify-syntax-entry ?> \")<\" )
   You can set hot keys to perform matching with one keystroke.
   Example: f6 and Control-C 6.
      (global-set-key \"\\C-c6\" 'match-parenthesis)
      (global-set-key [f6] 'match-parenthesis) "
     (interactive "p")
     (let ((syntax (char-syntax (following-char))))
       (cond
         ((= syntax ?\()
          (forward-sexp 1) (backward-char))
         ((= syntax ?\))
          (forward-char) (backward-sexp 1))
         (t (message "No match"))
       )
     )
)


;;;; Function Set for Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; switch wrap lines : on=>off / off=>on
(defun get-mode-name ()
  "Display `major-mode' and `mode-name'"
  (interactive)
  (message "Major-Mode:%s, Mode-Name:%s" major-mode mode-name))

(defun my-wrap-mode-on ()
  "Minor mode for making buffer not wrap long lines to next line."
  (interactive)
  (setq truncate-lines nil))

(defun my-wrap-mode-off ()
  "Minor mode for making buffer wrap long lines to next line."
  (interactive)
  (setq truncate-lines t))

(defun my-toggle-wrap-mode ()
  "Switch wrap mode from wrap to non-wrap, or vice-versa."
  (interactive)
  (if (eq truncate-lines nil)
      (my-wrap-mode-off)
    (my-wrap-mode-on)
  )
)

;;; === switch major mode ===
;; {{
(defvar switch-major-mode-last-mode nil)
(make-variable-buffer-local 'switch-major-mode-last-mode)

(defun major-mode-heuristic (symbol)
  (and (fboundp symbol) (string-match ".*-mode$" (symbol-name symbol)))
)

(defun my-switch-major-mode (mode)
  "Switch major mode"
  (interactive
    (let ((fn switch-major-mode-last-mode) val)
      (setq val
        (completing-read
          (if fn
            (format "Switch major mode to (default %s): " fn)
            "Switch major mode to: "
          )
          obarray 'major-mode-heuristic t nil nil (symbol-name fn)
        )
      )
      (list (intern val))
    )
  )
  (let ((last-mode major-mode))
    (funcall mode)
    (setq switch-major-mode-last-mode last-mode)
  )
)
;; }} end of switch major mode


;;;; Function Set for Buffer and File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reload-dotemacs ()
  "Reload ~/.emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el")
)

(defun sacha/byte-recompile ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0)
  )

;;; === load .emacs.elc ===
(defun my-reload-dotemacselc ()
  "Byte compiles and loads the .emacs.elc file."
  (interactive)
  (progn
    (byte-compile-file "~/.emacs.d/init.el")
    (load-file "~/.emacs.d/init.elc")
  )
)

(defun my-autocompile-dotemacs nil
  "Auto compile ~/.emacs when it's saved"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs.d/init.el")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
      (byte-compile-file dotemacs)
    )
  )
)
;; (add-hook 'after-save-hook 'my-autocompile-dotemacs)

;;; === save-buffer-kill-buffer ===
(defun my-save-buffer-kill-buffer (arg)
  "Saves buffer, if necessary (with ARG, w/o asking), and then kills it."
  (interactive "P")
  (let ((buf (current-buffer)))
    (if (and (buffer-file-name buf)
      (buffer-modified-p)
      (or arg (y-or-n-p (format "Save buffer %s? " (buffer-name)))))
      (save-buffer nil)
    )
    (delete-windows-on buf)
    (kill-buffer buf)
  )
)

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the file associated with the current buffer."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (concat "Really Delete File: " currentFile))
      (kill-buffer (currentFile))
      (delete-file currentFile)
      (message (concat "Delete File: " currentFile))
  ))
)

(defun delete-current-buffer-file ()
    "Removes file connected to current buffer and kills buffer."
    (interactive)
    (let ((filename (buffer-file-name))
          (buffer (current-buffer))
          (name (buffer-name)))
        (if (not (and filename (file-exists-p filename)))
            (ido-kill-buffer)
            (when (yes-or-no-p "Are you sure you want to remove this file? ")
                (delete-file filename)
                (kill-buffer buffer)
                (message "File '%s' successfully removed" filename)))))


(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace)))
)

;;; === kill-buffer-other-window ===
(defun my-kill-buffer-other-window (arg)
  "Kill the buffer in the ARGth other window, or the current buffer if no
other window."
  (interactive "p")
  (let ((buf (save-window-excursion
           (other-window arg)
           (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf)
  )
)

;;; === my-save-buffer-kill-frame ===
(defun my-save-buffer-kill-frame (arg)
  "Saves buffer, if necessary (with ARG, w/o asking), and then kills
it and its frame."
  (interactive "P")
  (let ((buf (current-buffer))
    (delete-frame nil)
    (kill-buffer nil))
    (if (and (buffer-file-name buf)
         (buffer-modified-p)
         (or arg (y-or-n-p (format "Save buffer %s? " (buffer-name)))))
    (save-buffer nil))
    (setq kill-buffer (or (not (buffer-modified-p buf))
              (not (buffer-file-name buf))
              (yes-or-no-p (concat "Buffer "
                           (buffer-name buf)
                           " modified; kill anyway? "))))
    (setq delete-frame (if (and (one-window-p)
                (or arg
                    (unwind-protect
                    (y-or-n-p "Delete frame as well? ")
                      (message ""))))
               (selected-frame)
             nil))
    (delete-windows-on buf)
    (if kill-buffer (progn (if (string-match "XEmacs" (emacs-version))
                   (set-buffer-modified-p nil buf)
                 (save-excursion
                   (set-buffer buf)
                   (set-buffer-modified-p nil)))
               (kill-buffer buf)))
    (and delete-frame (delete-frame))
  )
)

;;;###autoload
(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)" change)
                                (kill-buffer (process-buffer proc))))))))

;;;###autoload
(defun execute-command-on-file (file command)
  "execute COMMAND on FILE"
  (interactive
   (list (read-file-name "File execute command on: ")
         (let* ((input ""))
           (while (string= input "")
             (setq input (read-string "Command: ")))
           input)))
  (if file
      (when (yes-or-no-p (concat command " file `" file "'?"))
        (shell-command (concat command " \"" file "\"")))
    (message "Executing command `%s'..." command)
    (shell-command command)))

;;;###autoload
(defun execute-command-on-current-file (command)
  "execute COMMAND on current BUFFER, if the BUFFER is related with a FILE,
  then execute `revert-buffer-no-confirm'"
  (interactive
   (list (let* ((input ""))
           (while (string= input "")
             (setq input (read-string "Command: ")))
           input)))
  (let* ((file (buffer-file-name)))
    (execute-command-on-file file command)
    (if file
        (revert-buffer-no-confirm))))

;;;###autoload
(defun execute-command-on-current-dir (command)
  "execute COMMAND in current DIR."
  (interactive
   (list (let* ((input ""))
           (while (string= input "")
             (setq input (read-string "Command: ")))
           input)))
  (let* ((file (buffer-file-name)))
    (execute-command-on-file default-directory command)
    (if file
        (revert-buffer-no-confirm))))

;;;###autoload
(defmacro def-execute-command-on-file-command (command)
  "Make definition of command which execute command on file."
  `(defun ,(intern (subst-char-in-string ?\ ?- command)) (file)
     ,(concat "Run command `" command "' on file FILE.")
     (interactive (list (read-file-name (concat "File to " ,command ": "))))
     (execute-command-on-file file ,command)))

;;;###autoload
(defmacro def-execute-command-on-current-file-command (command)
  "Make definition of command which execute command on current file."
  `(defun ,(am-intern (subst-char-in-string ?\ ?- command) "-current-file") ()
     ,(concat "Execute command `" command "' on current file.")
     (interactive)
     (execute-command-on-current-file ,command)))

;;;###autoload
(defmacro def-execute-command-on-current-dir-command (command)
  "Make definition of command which execute command on current directory."
  `(defun ,(am-intern (subst-char-in-string ?\ ?- command) "-current-dir") ()
     ,(concat "Execute command `" command "' on current directory.")
     (interactive)
     (execute-command-on-current-dir ,command)))

(defun my-file-executable-p (file)
  "Make sure the file FILE exists and is executable."
  (if file
    (if (file-executable-p file)
          file
        (message "WARNING: Can't find executable `%s'" file)
        ;; sleep 1 s so that you can read the warning
        (sit-for 1))
    (error "my-file-executable-p: missing operand")
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-make-directory-yes-or-no (dir)
  "Ask user to create the DIR, if it does not already exist."
  (if dir
    (if (not (file-directory-p dir))
          (if (yes-or-no-p (concat "The directory `" dir
                                   "' does not exist currently. Create it? "))
              (make-directory dir t)
            (error
             (concat "Cannot continue without directory `" dir "'"))))
    (error "my-make-directory-yes-or-no: missing operand")
  )
)

(defun last-command-other-window()
  "Execute last command in other window."
  (interactive)
  (unless (memq last-command
                '(last-command-other-window
                  eval-defun
                  ))
    (other-window 1)
    (funcall last-command)
    (other-window 1)))

;; To overcome the fact that that we can't bytecompile the .emacs while it is being read
;;  (when (file-newer-than-file-p "~/.emacs" "~/.emacs.elc")
;;    (defun byte-compile-dotemacs-if-needed ()
;;      (when (y-or-n-p "byte-compiling .emacs to .emacs.elc?")
;;        (byte-compile-file "~/.emacs")
;;      )
;;      (remove-hook 'find-file-hooks 'byte-compile-dotemacs-if-needed)
;;    )
;;    (add-hook 'find-file-hooks 'byte-compile-dotemacs-if-needed)
;;  )

;;;###autoload
(defalias 'apply-define-key 'eal-define-keys-commonly)
;;;###autoload
(defalias 'define-key-list 'eal-define-keys-commonly)

;;;###autoload
(defun apply-args-list-to-fun (fun-list args-list)
  "Apply args list to function FUN-LIST.
FUN-LIST can be a symbol, also can be a list whose element is a symbol."
  (let ((is-list (and (listp fun-list) (not (functionp fun-list)))))
    (dolist (args args-list)
      (if is-list
          (dolist (fun fun-list)
            (apply-args-to-fun fun args))
        (apply-args-to-fun fun-list args)))))

;;;###autoload
(defun apply-args-to-fun (fun args)
  "Apply args to function FUN."
  (if (listp args)
      (eval `(,fun ,@args))
    (eval `(,fun ,args))))

;;;###autoload
(defun list-colors-display-htm (&optional list)
  "Create HTML page which lists all the defined colors."
  (interactive)
  (if (and (null list) window-system)
      (progn
        (setq list (x-defined-colors))
        ;; Delete duplicate colors.
        (let ((l list))
          (while (cdr l)
            (if (facemenu-color-equal (car l) (car (cdr l)))
                (setcdr l (cdr (cdr l)))
              (setq l (cdr l)))))))
  (with-output-to-temp-buffer "*Colors*"
    (save-excursion
      (set-buffer standard-output)
      (insert "<html>\n"
              "<head>\n"
              "<meta http-equiv=\"Content-Style-Type\" content=\"text/css\">\n"
              "<title>Colors</title>\n"
              "</head>\n"
              "<body>\n"
              "<h1>Colors</h1>\n"
              "<p>\n"
              "<pre>\n")
      (let (s)
        (while list
          (insert (format (concat "<span style=\"background-color:%s\">%-20s</span>"
                                  "  "
                                  "<span style=\"color:%s\">%s</span>"
                                  "\n")
                          (html-color (car list)) (car list)
                          (html-color (car list)) (car list)))
          (setq list (cdr list))))
      (insert "</pre>"
              "</body>"
              "</html>"))))

;;;###autoload
(defun html-color (string)
  "Convert colors names to rgb(n1,n2,n3) strings."
  (format "rgb(%d,%d,%d)"
          (/ (nth 0 (x-color-values string)) 256)
          (/ (nth 1 (x-color-values string)) 256)
          (/ (nth 2 (x-color-values string)) 256)))

;;;###autoload
(defmacro def-command-max-window (command)
  "Make definition of command which after execute command COMMAND
   execute `delete-other-windows'."
  `(defun ,(am-intern command "-max-window") ()
     ,(concat "After run command `" command "' execute command `delete-other-windows'.")
     (interactive)
     (call-interactively ',(intern command))
     (delete-other-windows)))

;;;###autoload
(defun delete-current-window (&optional frame)
  "Delete window which showing current buffer."
  (interactive
   (list (and current-prefix-arg
              (or (natnump (prefix-numeric-value current-prefix-arg))
                  'visible))))
  (if (one-window-p)
      (bury-buffer)
    (delete-windows-on (current-buffer) frame)))

;;;###autoload
(defmacro def-turn-on (command &optional is-on)
  "Make definition of command whose name is COMMAND-on when IS-ON is t
   and COMMAND-off when IS-ON is nil."
  (let ((on (if is-on "on" "off")))
    `(defun ,(am-intern command "-" on) ()
       ,(concat "Turn " on " `" command "'.")
       (interactive)
       (funcall ',(intern command) ,(if is-on 1 -1)))))

;;;###autoload
(defun unset-key (keymap key)
  "Remove binding of KEY in map KEYMAP.
   KEY is a string or vector representing a sequence of keystrokes."
  (define-key keymap key nil))

;;; === count characters in a region ===
(defun my-count-words-in-region (beginPos endPos)
  "Print number of words and chars in region."
  (interactive "r")
  (message "Counting ...")
  (save-excursion
    (let (wCnt charCnt)
      (setq wCnt 0)
      (setq charCnt (- endPos beginPos))
      (goto-char beginPos)
      (while (and (< (point) endPos)
                  (re-search-forward "\\w+\\W*" endPos t))
             (setq wCnt (1+ wCnt))
      )
      (message "Words: %d. Chars: %d." wCnt charCnt)
    )
  )
)

;;; === update number ===
;; similar to "C-a" in vim
;;;###autoload
(defun ywb-html-preview-region (beg end)
  (interactive "r")
  (let ((file (make-temp-file "region-" nil ".html")))
    (write-region beg end file)
    (browse-url file)
  )
)
(defvar wcy-rotate-text-definations
  '(("[0-9]+" . (lambda (arg)
                  (format "%d" (+ arg (string-to-number (match-string 0))))))
    ("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
  " a list of ROT text defination. each element is a defination.
    element can be a list of string or a cons. (REGEXP . func)
    if REGEXP matched, func is called with no args, return value is the next value."
)
(defun wcy-rotate-text-aux (arg)
  (catch 'break
    (mapc
     #'(lambda (def)
         (let ((regexp (if (functionp (cdr def))
                           (car def)
                         (mapconcat 'regexp-quote def "\\|")))
               (func (if (functionp (cdr def))
                         (cdr def)
                       #'(lambda (arg)
                           (let* ((len (length def))
                                  (rest (member (match-string 0) def))
                                  (pos (- len (length rest))))
                             (format "%s" (nth (mod (+ pos arg) len) def)))))))
           (if (re-search-forward regexp (line-end-position) t nil)
               (throw 'break (funcall func arg)))))
     wcy-rotate-text-definations)
    nil)
)

(defun my-rotate-text(arg)
  (interactive "p")
  (save-excursion
    (let ((x (wcy-rotate-text-aux arg)))
      (if x (replace-match x))
    )
  )
)
;;; ==== end of update number

;;; === save line ===
(defun my-kill-ring-save-line ()
  "Add current line to kill-ring "
  (interactive)
  (progn
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (kill-new (buffer-substring beg (point)))
    )
  )
)

;;; === open dired ===
(defun my-dired-open-in-current-buffer ()
  "Open the currently selected file/directory in the same buffer as this one."
  (interactive)
  (find-alternate-file (dired-get-filename))
)

;;; === Set The Size and Position of Emacs Frames ===
(defun my-arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)
  )
)
;; (my-arrange-frame 70 80 2 22)

;;; === add executable to some files ===
(setq my-shebang-patterns
      (list "^#!/usr/.*/perl\\(\\( \\)\\|\\( .+ \\)\\)-w *.*"
            "^#!/usr/.*/python\\(\\( \\)\\|\\( .+ \\)\\)-w *.*"
            "^#!/usr/.*/sh"
            "^#!/usr/.*/csh"
            "^#!/usr/.*/csh -f"
            "^#!/usr/.*/bash"
            "^#!/bin/sh"
            "^#!/.*/perl"
            "^#!/.*/awk"
            "^#!/.*/sed"
            "^#!/bin/bash")
)
(add-hook 'after-save-hook
  (lambda ()
    (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
      (progn
        ;;This puts message in *Message* twice, but minibuffer output looks better
        (message (concat "Wrote " (buffer-file-name)))
        (save-excursion
          (goto-char (point-min))
          ;; Always checks every pattern even after match. Inefficient but easy
          (dolist (my-shebang-pat my-shebang-patterns)
            (if (looking-at my-shebang-pat)
              (if (= (shell-command
                (concat "chmod u+x " (buffer-file-name)))
                 0)
                (message (concat "Wrote and made executable " (buffer-file-name)))
              )
            )
          )
        )
      )
     ;; This puts message in *Message* twice, but minibuffer output looks better
     (message (concat "Wrote " (buffer-file-name)))
    )
  )
)

;; popup a terminal
(defun my-popup-term ()
  (interactive)
  (apply 'start-process "terminal" nil popup-terminal-command)
)

;; repeat last command passed to "shell-mode"
(defun repeat-shell-command ()
  "Repeat most recently executed shell command."
  (interactive)
  (save-buffer)
  (or shell-command-history (error "Nothing to repeat."))
  (shell-command (car shell-command-history)))
;; (global-set-key (kbd "C-c j") 'repeat-shell-command)

(defun his-imenu()
  "Call imenu, showing completions."
  (interactive)
  (setq unread-command-events (list 9))
  (imenu (imenu-choose-buffer-index)))

;;  save your last macro by typing M-x save-macro
(defun save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro
     (kmacro-name-last-macro name)         ; use this name for the macro
     (find-file "~/.emacs")                ; open the .emacs file
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (switch-to-buffer nil)                ; return to the initial buffer
     )

;; insert 128-bit random number
(defun my-insert-rand128 ()
  "Insert 128-bit random number (in hex) at point."
  (interactive)
  (shell-command
   "dd if=/dev/urandom count=1 2>/dev/null | md5sum -b | cut -d' ' -f1 | tr -d '\n' "
   (quote (4)) nil)
  )

;; note: may use delete-trailing-whitespace
(defun my-delete-trailing-spaces ()
  "Delete trailing spaces (or tabs) in all lines."
  (interactive)
  (progn
    (save-excursion
      (goto-line 1) (replace-regexp "[ \t]+$" "") ) ) )

(defun my-nsplit-line (n)
  "Split line into pieces of length N."
  (interactive "nSplit into pieces of length n: ")
  (progn
    (if (< n 1) (error "n must be greater than zero"))
    (let ((beg) (end) (stp))
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point))
      (setq stp (point))
      (while (< beg end)
        (goto-column n)
        (insert "\n")
        (setq beg (+ n beg)) )
      (goto-char stp) )
    )
)

;;
(defun my-count-matches-region (r)
  "Count occurences of REGEXP in region."
 (interactive "s Enter regexp: ") ; elips.ps.gz p.335
  (progn
;;    (message "%d" (region-beginning))
;;    (message "%d" (region-end))
;;    (message "%S" r)
    (how-many r (region-beginning) (region-end))
    )
  )

(defun toggle-line-spacing ()
"Toggle line spacing between 1 and 5 pixels."
    (interactive)
    (if (eq line-spacing 1)
        (setq-default line-spacing 5)
        (setq-default line-spacing 1))
    (redraw-display)
)

(defun new-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline))
  (indent-for-tab-command))

(defun new-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

(defun insert-line-number-for-region (beg end &optional start-line)
  "Insert line numbers into region."
  (interactive "r")
  (save-excursion
    (let ((max (count-lines beg end))
          (line (or start-line 1))
          (counter 1))
      (goto-char beg)
      (while (<= counter max)
        (insert (format "%0d " line))
        (beginning-of-line 2)
        (incf line)
        (incf counter)))))

(defun insert-line-number-for-buffer ()
  "Insert line number into buffer."
  (interactive)
  (if mark-active
      (insert-line-number (region-beginning) (region-end) (read-number "Start line: "))
    (insert-line-number (point-min) (point-max))))

(defun my-create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ;; (lisp-interactive-mode)
    ))

(defun my-smart-split-helper(w)
  "Helper function to split a given window into two, the first of which has
     80 columns."
  (if (> (window-width w) (* 2 81))
      (let ((w2 (split-window w 82 t)))
        (my-smart-split-helper w2))))

(defun my-smart-split()
  "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns."
  (interactive)
  (smart-split-helper nil))

;; replace(refresh) current buffer text with the text of the visited file on disk
(defun my-revert-buffer ()
  "Unconditionally revert current buffer."
  (interactive)
  ;; (flet ((yes-or-no-p (msg) t))
  (cl-flet ((yes-or-no-p (msg) t))
    (revert-buffer))
)

(defun my-revert-all-buffers()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun revert-all-buffers ()
    "Revert all non-modified buffers associated with a file.
  This is to update existing buffers after a Git pull of their underlying files."
    (interactive)
    (save-current-buffer
        (mapc (lambda (b)
                 (set-buffer b)
                 (unless (or (null (buffer-file-name)) (buffer-modified-p))
                    (revert-buffer t t)
                    (message "Reverted %s\n" (buffer-file-name))))
              (buffer-list))))

;; open file with sudo.
(defun my-find-file-with-sudo()
  "open a file with sudo"
  (interactive)
  (let ((fname (or buffer-file-name
                   dired-directory)))
    (when fname
      (if (string-match "^/sudo root@localhost:" fname)
          (setq fname (replace-regexp-in-string
                       "^/sudo root@localhost:" ""
                       fname))
          (setq fname (concat "/sudo root@localhost:" fname)))
      (find-alternate-file fname))))

;; Function to set up my default color combination.
(defun my-set-colors()
  "Set my favorite color combination."
  (interactive)
  (if (or (eq window-system 'x)
          (eq window-system 'ns)
          (eq window-system 'w32))
      (set-background-color "Black")
      (set-foreground-color "Gray")
      (set-cursor-color "Gray")))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun my-rename-file-and-buffer(new-name)
  "Renames both current buffer and file it's visiting to new-name."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
            (progn
              (rename-file name new-name 1)
              (rename-buffer new-name)
              (set-visited-file-name new-name)
              (set-buffer-modified-p nil))))))

(defun rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let ((name (buffer-name))
            (filename (buffer-file-name)))
         (if (not (and filename (file-exists-p filename)))
             (error "Buffer '%s' is not visiting a file!" name)
             (let ((new-name (read-file-name "New Filename: " filename)))
                  (if (get-buffer new-name)
                      (error "A buffer named '%s' already exists!" new-name)
                      (rename-file filename new-name 1)
                      (rename-buffer new-name)
                      (set-visited-file-name new-name)
                      (set-buffer-modified-p nil)
                      (message "File '%s' successfully renamed to '%s'"
                                name (file-name-nondirectory new-name)))))))

;;  Never understood why Emacs doesn't have this function, either.
(defun my-move-buffer-file(dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
            (if (string-match dir "\\(?:/\\|\\\\)$")
                (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
        (progn
          (copy-file filename newname 1)
          (delete-file filename)
          (set-visited-file-name newname)
          (set-buffer-modified-p nil)
          t))))


;; go to last edit position
(defun my-goto-last-edit-pos ()
  "Go to the last position where editing occurred."
  (interactive)
  (let ((undos buffer-undo-list))
    (when (listp undos)
      (while
        (and undos
          (let
            ((pos (or (cdr-safe (car undos)) (car undos))))
            (not (and (integerp pos) (goto-char (abs pos))))
          )
        )
        (setq undos (cdr undos))
      )
    )
  )
)

;;; === get column number ===
(defun my-get-col ()
  "Return column number of point."
  (interactive)
  (save-excursion
    (let ( (opoint (point)) (ncol) )
      (beginning-of-line)
          (setq ncol (- opoint (point)))
;;          (message "col = %s" ncol)
          ncol
    )
  )
)


;;; === go to char ===
;;;###autoload
(defun my-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `my-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to Char:")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event))
)

;;; === Goto column n ===
(defun my-goto-column (n)
  "Goto column ARG, counting from column 0. Argument N is column number."
  (interactive "Goto Column: ")
  (move-to-column n)
)


(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be PHP, Perl, Python, Ruby, javascript, Bash, ocaml, vb, elisp.
File suffix is used to determine what program to run."
(interactive)
  (let (suffixMap fName suffix progName cmdStr)

    ;; a keyed list of file suffix to comand-line program path/name
    (setq suffixMap
          '(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "js")
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
;            ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            )
          )

    (setq fName (buffer-file-name))
    (setq suffix (file-name-extension fName))
    (setq progName (cdr (assoc suffix suffixMap)))
    (setq cmdStr (concat progName " \""   fName "\""))

    (if (string-equal suffix "el") ; special case for emacs lisp
        (load-file fName)
      (if progName
        (progn
          (message "Running…")
          (shell-command cmdStr "*run-current-file output*" )
          )
        (message "No recognized program file suffix for this file.")
        )
)))

;; open my Gnus configuration file
(defun my-open-dot-gnus ()
    "Opening `~/.gnus’"
    (interactive)
    (find-file “~/.gnus”))

(defun my-send-current-line-to-next-window ()
  "Send current line to next window"
  (interactive)
  (let ((current-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (target (window-buffer (next-window))))
    (with-current-buffer target
      (insert current-line))))

(defun copy-to-end-of-line ()
  (interactive)
  (copy-region-as-kill (point) (point-at-eol)))

(defun new-line-in-normal-mode ()
  "make a new line without moving the cursor or leaving normal mode"
  (interactive)
  (save-excursion
    (evil-insert-newline-below)
    (evil-force-normal-state)))

(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))


(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (eproject-root)))
    (shell-command (concat "ctags -e -R --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun build-gtags ()
  (interactive)
  (message "building gtags")
  (let ((root (eproject-root)))
    (shell-command (concat "(cd " root " && gtags)"))
    (message "tags loaded")))

(defun my-find-tag ()
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
      (visit-project-tags)
    (build-ctags))
  (etags-select-find-tag-at-point))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

;;; select between parens
(defun select-in-parens ()
  (interactive)
  (set-mark (point))
  (goto-match-paren 1))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun file-name-at-point ()
  (save-excursion
    (let* ((file-name-regexp "[./a-zA-Z0-9\-_~]")
           (start (progn
                    (while (looking-back file-name-regexp)
                      (forward-char -1))
                    (point)))
           (end (progn
                  (while (looking-at file-name-regexp)
                    (forward-char 1))
                  (point))))
      (buffer-substring start end))))

;; Fix all indentation
(defun hjking-fix-indentation ()
    "indent whole buffer"
    (interactive)
    (delete-trailing-whitespace)
    ;(replace-string "if(" "if (")
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max)))

;; reindent the entire buffer
(defun reindent-buffer ()
  "Reindent the whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my/clean-buffer-formatting ()
  "Indent and clean up the buffer"
  (interactive)
  (indent-region (point-min) (point-max))
  (whitespace-cleanup))

(defun cycle-windows()
    "cycle the buffer of the windows in cyclic ordering"
    (interactive)
    (mapcar  (lambda(window)
        (let ((next-window-buffer (window-buffer (next-window window 0))))
            (set-window-buffer (next-window window 0) (window-buffer window))
            (set-window-buffer window next-window-buffer))) (butlast (window-list nil 0))))

(defun sacha/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
    (package-install package))))

(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun sacha/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))
(global-set-key (kbd "M-#")         'sacha/search-word-backward)
(global-set-key '[M-*] 'sacha/search-word-forward)
(defadvice search-for-keyword (around sacha activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun my-shift-region (numcols)
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols))))

;; Smart home key
(defun my-smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; Joining lines
;; https://github.com/rejeep/emacs/blob/master/rejeep-defuns.el#L150-L158
(defun join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

;;; Google
(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Search Google: "))))))

;;; Percentage-buffer
(defun goto-percent (pct)
  "Go to place in a buffer expressed in percentage."
  (interactive "nPercent: ")
  (goto-char (/ (* (point-max) pct) 100)))

;; Count total number of words in current buffer
(defun count-words-buffer ()
  "Count total number of words in current buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
  (forward-word 1)
  (setq count (1+ count)))
      (if (zerop count)
    (message "buffer has no words.")
  (message "buffer approximately has %d %s." count
     (pluralize "word" count))))))

;;; Highlight-annotations
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for
programming."
  (font-lock-add-keywords
   nil '(("\<\(FIX\(ME\)?\|TODO\|OPTIMIZE\|HACK\|REFACTOR\):"
          1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; Popup Help
(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (with-temp-buffer
                        (help-mode)
                        (help-xref-interned thing)
                        (buffer-string))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))


(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun delete-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^([<>“") (setq p1 (point))
      (skip-chars-forward "^)]<>”") (setq p2 (point))
      (delete-region p1 p2))))

;; print the key bindings in a tabular form
;; [from http://www-xray.ast.cam.ac.uk/~gmorris/dotemacs.html]

(defun my-keytable (arg)
    "Print the key bindings in a tabular form."
    (interactive "sEnter a modifier string:")
    (with-output-to-temp-buffer "*Key table*"
    (let* ((i 0)
            (keys (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                        "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                        "<return>" "<down>" "<up>" "<right>" "<left>"
                        "<home>" "<end>" "<f1>" "<f2>" "<f3>" "<f4>" "<f5>"
                        "<f6>" "<f7>" "<f8>" "<f9>" "<f10>" "<f11>" "<f12>"
                        "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
                        "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-"
                        "_" "=" "+" "\\" "|" "{" "[" "]" "}" ";" "'" ":"
                        "\"" "<" ">" "," "." "/" "?"))
            (n (length keys))
            (modifiers (list "" "S-" "C-" "M-" "M-C-"))
            (k))
        (or (string= arg "") (setq modifiers (list arg)))
        (setq k (length modifiers))
        (princ (format " %-10.10s |" "Key"))
        (let ((j 0))
        (while (< j k)
            (princ (format " %-28.28s |" (nth j modifiers)))
            (setq j (1+ j))))
        (princ "\n")
        (princ (format "_%-10.10s_|" "__________"))
        (let ((j 0))
        (while (< j k)
            (princ (format "_%-28.28s_|"
                            "_______________________________"))
            (setq j (1+ j))))
        (princ "\n")
        (while (< i n)
        (princ (format " %-10.10s |" (nth i keys)))
        (let ((j 0))
            (while (< j k)
            (let* ((binding
                    (key-binding (read-kbd-macro (concat (nth j modifiers)
                                                            (nth i keys)))))
                    (binding-string "_"))
                (when binding
                (if (eq binding 'self-insert-command)
                    (setq binding-string (concat "'" (nth i keys) "'"))
                    (setq binding-string (format "%s" binding))))
                (setq binding-string
                    (substring binding-string 0 (min (length
                                                        binding-string) 28)))
                (princ (format " %-28.28s |" binding-string))
                (setq j (1+ j)))))
        (princ "\n")
        (setq i (1+ i)))
        (princ (format "_%-10.10s_|" "__________"))
        (let ((j 0))
        (while (< j k)
            (princ (format "_%-28.28s_|"
                            "_______________________________"))
            (setq j (1+ j))))))
    (delete-window)
    (hscroll-mode)
    (setq truncate-lines t))
