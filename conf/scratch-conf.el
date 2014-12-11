;; Scratch buffer goodness

;; make *scratch* buffer clean
(setq initial-scratch-message "")
;;; I initialize my *scratch* buffer with a random Emacs haiku drawn
;;; from among these:
(defvar ted-emacs-haiku
  '("Oort is so awesome
     deuglifies Outlook crap
     `W k' rocks"
    "Great clouds overhead
     Tiny black birds rise and fall
     Snow covers Emacs
         -- Alex Schroeder"
    "hacking on Smyrno
     `error in process filter'
     something is b0rken"
    "Swiftly typing. Oh!
     Where would we be without you,
     `self-insert-command'?"
    "treeless quiet field
     sudden bud: EmacsWiki
     now he{ar,re} the birds sing
         -- ttn"
    "an emacs user's
     fingers dance on the keyboard;
     a nerd pianist
         -- Erik Bourget"
    "The file was open.
     flying in a sparrow stole
     a parenthesis
         -- Oliver Scholz"
    "The day went away.
     The file still puts its weight on
     the tired mode-line.
         -- Oliver Scholz"
    "On a cloudy day
     you hear the cons cells whisper:
     'We are lost and gone.'
         -- Oliver Scholz"
    "A message, a string
     remind me of my sweet love.
     Good bye, my buffers.
         -- Oliver Scholz"
    "Hot night in summer:
     Hush, you quibbling characters!
     Do not wake her up!
         -- Oliver Scholz"
    "A bright, busy day.
     The windows watch a thousand
     wild cursors dancing.
         -- Oliver Scholz"
    "Oh, why don't you are
     a lake, a stream, a meadow
     this morning, Emacs?
         -- Oliver Scholz" ;%
    "The friends chat gaily,
     I stand up to join their talk.
     My `save-excursion'.
         -- Oliver Scholz")
  "Haiku taken from the Emacs Wiki's EmacsHaiku page.")

(defun ted-random-emacs-haiku (&optional prefix)
  "Select and format a random haiku from `ted-emacs-haiku'."
  (random t)
  (let* ((prefix (or prefix ";; "))
         (n (random (length ted-emacs-haiku)))
         (haiku (nth n ted-emacs-haiku)))
    (with-temp-buffer
      (insert haiku)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (goto-char (point-at-bol))
        (delete-horizontal-space)
        (insert prefix)
        (when (looking-at "--")
          (insert "    "))
        (forward-line 1))
      (concat (buffer-substring-no-properties (point-min) (point-max))
              "\n\n"))))

;; If the *scratch* buffer is killed, recreate it automatically
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (insert initial-scratch-message)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

(defun totd ()
  "Show a tip everytime start emacs"
  (interactive)
  (with-output-to-temp-buffer "*Tip of The Day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))

;;  (defun my-startup-fcn ()
;;      "do fancy things"
;;      (let ((my-buffer (get-buffer-create "my-buffer")))
;;          (with-current-buffer my-buffer
;;              ;; this is what you customize
;;              (insert "some stuff\nmore stuff"))
;;          (switch-to-buffer my-buffer)))

(defvar scratch-file-name (concat my-personal-dir ".scratch"))

(defun save-scratch-data ()
   (let ((str (progn
            (set-buffer (get-buffer "*scratch*"))
            (buffer-substring-no-properties
            (point-min) (point-max))))
          (file scratch-file-name))
        (if (get-file-buffer (expand-file-name file))
            (setq buf (get-file-buffer (expand-file-name file)))
            (setq buf (find-file-noselect file)))
            (set-buffer buf)
            (erase-buffer)
            (insert str)
            (save-buffer)))

(defun read-scratch-data ()
    "Load scratch from a file"
    (let ((file scratch-file-name))
        (when (file-exists-p file)
        (set-buffer (get-buffer "*scratch*"))
        (erase-buffer)
        (insert-file-contents file))))

;; initial-buffer-choice: t, open *scratch* buffer
;;      string, visit the spcified file or dir
;; (setq initial-buffer-choice "~/.emacs.d/org/todo.org")

;; controls the text that appears in the *scratch* buffer
;; (setq initial-scratch-message (ted-random-emacs-haiku))

(defun my-close-scratch ()
    "Kill *scratch* buffer"
   (kill-buffer "*scratch*")
   ;; (if (not (delq nil (mapcar 'buffer-file-name (buffer-list))))
   ;;     ;; (new-untitled-buffer)
   ;;     (switch-to-buffer "*Ibuffer*")
   ;; )
)
(defun my-emacs-startup-hook ()
   (my-close-scratch))

;; do whatever you want after Emacs starts up
;; (add-hook 'emacs-startup-hook 'my-startup-fcn)
;; (add-hook 'emacs-startup-hook 'my-emacs-startup-hook)
(add-hook 'emacs-startup-hook 'read-scratch-data)

(defun my-switch-to-ibuffer ()
    "Switch to *Ibuffer*"
    (switch-to-buffer "*Ibuffer*")
)
(defun my-switch-to-messages ()
    "Switch to *Messages* buffer"
    (switch-to-buffer "*Messages*")
)
(defun my-emacs-after-init-hook ()
    "Function for after-init-hok"
    (totd)
    (my-close-scratch)
    ;; (my-switch-to-ibuffer)
    (my-switch-to-messages)
)
;; (add-hook 'after-init-hook 'my-emacs-after-init-hook)
;; (add-hook 'after-init-hook 'my-startup-buffer)

;; save scratch when exit emacs
;; (add-hook 'kill-emacs-hook (lambda () (save-scratch-data)))

(provide 'scratch-conf)