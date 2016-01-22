;;
;; ido
;;

(use-package ido
  :preface (progn
    ;; `defvar's to prevent compile warnings
    (defvar ido-cur-item               nil)
    (defvar ido-default-item           nil)
    (defvar predicate                  nil)
    (defvar inherit-input-method       nil)
    (defvar ido-cur-list               nil)
    (defvar ido-context-switch-command nil))
  :init (progn
    (setq ido-enable-flex-matching  t) ; enable fuzzy search
    (setq ido-everywhere            t)
    (setq ido-create-new-buffer     'always) ; create a new buffer if no buffer matches substring
    (setq ido-save-directory-list-file (concat my-cache-dir "ido_last"))
    ;; Allow the same buffer to be open in different frames
    (setq ido-default-buffer-method 'selected-window)
    (setq ido-create-new-buffer 'always)
    (setq ido-enable-regexp t)
    (setq ido-completion-buffer-all-completions t)
    (setq ido-enable-flex-matching t)
    (setq ido-record-commands t)
    (setq ido-max-directory-size 100000)
    ;; Show previously opened buffers in ido-switch-buffer
    (setq ido-use-virtual-buffers t)
    (setq ido-case-fold t)
    (setq ido-enable-last-directory-history t)
    (setq ido-buffer-history t)
    (setq ido-max-work-directory-list 100)
    (setq ido-max-work-file-list 50)
    ;; If thing-at-point looks like something openable, default to it
    (setq ido-use-filename-at-point 'guess)
    (setq ido-use-url-at-point t)
    (setq ido-max-prospects 10)
    (setq ido-enable-tramp-completion nil)
    (setq ido-confirm-unique-completion nil)
    (setq ido-enable-prefix nil)
    ;;
    (setq ido-default-file-method 'selected-window)

    ;;; Set buffer separator in the mini buffer when press C-x b (ido-switch-buffer)
    ;;; to new line instead of the character | so that it can be easy to read
    (setq ido-decorations
          '("\n=> " "" "\n" "" "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
    (setq ido-file-extensions-order '(".sv" ".v" ".svh" ".tv" ".m" ".c" ".cpp" ".el" ".org" ".txt" ".py" ".emacs" ".xml" ".ini" ".cfg" ".cnf"))
    ;;  customize the order in which files are sorted when Ido displays them in
    ;; the minibuffer. There are certain file extensions I use more than others,
    ;; so I tell Ido to emphasize those

    ;; look into other directories if the entered filename doesn't exist
    ;; in current directory ido-auto-merge-work-directories-length -1
    ;; do NOT look into other directories if the entered filename doesn't
    ;; exist in current directory
    (setq ido-auto-merge-work-directories-length nil))
  :config (progn
    ;; Disable ivy
    (with-eval-after-load 'ivy
      (ivy-mode -1))
    (ido-mode 1)

    ;; Use flx-ido for better flex matching between words
    ;; Try out flx-ido for better flex matching between words
    ;; provides fuzzy completion to select completion candidates.
    ;; For example, if you want to select a file src/foo/bar.txt,
    ;; you only need to type in Ido prompt "sfb", short for (s)rc/(f)oo/(b)ar
    (use-package flx-ido
      :preface
      (progn
        (defvar flx-ido-mode nil))
      :config
      (progn
        (setq ido-use-faces nil) ; disable ido faces to see flx highlights
        (flx-ido-mode 1)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ido-vertical-mode
    ;; Makes ido-mode display vertically.
    (use-package ido-vertical-mode
      :disabled t
      :init (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
      :config
      (progn
       (ido-vertical-mode)
       ))

    (use-package ido-ubiquitous
      :preface
      (progn
        (defvar ido-ubiquitous-debug-mode nil))
      :config
      (progn
        (ido-ubiquitous-mode 1)))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ido-at-point
    ;; an alternative frontend for `completion-at-point'
    ; (require 'ido-at-point)
    ; (ido-at-point-mode)
    (use-package ido-at-point
       :config
        (ido-at-point-mode t))

    ;; Sometimes when using ido-switch-buffer the *Messages* buffer get in the way,
    ;; so we set it to be ignored (it can be accessed using `C-h e', so there is
    ;; really no need for it in the buffer list).
    ;; https://github.com/larstvei/dot-emacs
    (add-to-list 'ido-ignore-buffers "*Messages*")

    ;; Sort ido filelist by mtime instead of alphabetically
    ;; http://www.emacswiki.org/emacs/InteractivelyDoThings
    (defun modi/ido-sort-mtime ()
      ;; Sort the list only when the File gid is non-zero;
      ;; otherwise the sort function errors out.
      ;; (message "Current dir: %s File GID: %s File Attributes: %s"
      ;;          ido-current-directory
      ;;          (fourth (file-attributes ido-current-directory))
      ;;          (file-attributes ido-current-directory))
      (when (file-exists-p ido-current-directory) ; only if the current directory exists
        (when (not (= (fourth (file-attributes ido-current-directory)) 0))
          (setq ido-temp-list
                (sort ido-temp-list
                      (lambda (a b)
                        (time-less-p
                         (sixth (file-attributes (concat ido-current-directory b)))
                         (sixth (file-attributes (concat ido-current-directory a))))))))
        (ido-to-end  ; move . files to end (again)
         (delq nil (mapcar
                    (lambda (x) (and (char-equal (string-to-char x) ?.) x))
                    ido-temp-list)))))
    ; (add-hook 'ido-make-file-list-hook #'modi/ido-sort-mtime)
    ; (add-hook 'ido-make-dir-list-hook  #'modi/ido-sort-mtime)

    ;; Bind `~` to go to homedir when in ido-find-file;
    ;; http://whattheemacsd.com/setup-ido.el-02.html
    (add-hook 'ido-setup-hook
      (lambda ()
        ;; Go straight home
        (define-key ido-file-completion-map
          (kbd "~")
          (lambda ()
            (interactive)
            (if (looking-back "/")
                (insert "~/")
                (call-interactively 'self-insert-command))))))

    (defun endless/ido-bury-buffer-at-head ()
      "Bury the buffer at the head of `ido-matches'.
      http://endlessparentheses.com/Ido-Bury-Buffer.html
      This is merged into emacs 25.0."
      (interactive)
      (let ((enable-recursive-minibuffers t)
            (buf (ido-name (car ido-matches)))
            (nextbuf (cadr ido-matches)))
        (when (get-buffer buf)
          ;; If next match names a buffer use the buffer object;
          ;; buffer name may be changed by packages such as
          ;; uniquify.
          (when (and nextbuf (get-buffer nextbuf))
            (setq nextbuf (get-buffer nextbuf)))
          (bury-buffer buf)
          (if (bufferp nextbuf)
              (setq nextbuf (buffer-name nextbuf)))
          (setq ido-default-item nextbuf
                ido-text-init ido-text
                ido-exit 'refresh)
          (exit-minibuffer))))

    (defun ido-define-keys ()
      (unbind-key "C-a" ido-completion-map) ; default binding: `ido-toggle-ignore'
      (bind-keys
       :map ido-completion-map
        ;; C-n/p  and up/down keys are more intuitive in vertical layout
        ("C-n"    . ido-next-match)
        ("<down>" . ido-next-match)
        ("C-p"    . ido-prev-match)
        ("<up>"   . ido-prev-match)
        ("C-f"    . ido-magic-forward-char)
        ("C-b"    . ido-magic-backward-char)
        ("C-i"    . ido-toggle-ignore))
      (>=e "25.0"
           (bind-key "C-S-b" #'ido-bury-buffer-at-head ido-completion-map) ; emacs >= 25.0
           (bind-key "C-S-b" #'endless/ido-bury-buffer-at-head ido-completion-map))) ; emacs < 25.0
    ; (add-hook 'ido-setup-hook #'ido-define-keys)
    )
)

(provide 'ido-conf)

;; Default Ido Key Map
;;
;; Basic map
;; | C-a     | 'ido-toggle-ignore              |
;; | C-c     | 'ido-toggle-case                |
;; | C-e     | 'ido-edit-input                 |
;; | Tab     | 'ido-complete                   |
;; | Space   | 'ido-complete-space             |
;; | C-j     | 'ido-select-text                |
;; | C-m     | 'ido-exit-minibuffer            |
;; | C-p     | 'ido-toggle-prefix (OVERRIDDEN) |
;; | C-r     | 'ido-prev-match                 |
;; | C-s     | 'ido-next-match                 |
;; | C-t     | 'ido-toggle-regexp              |
;; | C-z     | 'ido-undo-merge-work-directory  |
;; | C-Space | 'ido-restrict-to-matches        |
;; | M-Space | 'ido-take-first-match           |
;; | C-@     | 'ido-restrict-to-matches        |
;; | Right   | 'ido-next-match                 |
;; | Left    | 'ido-prev-match                 |
;; | ?       | 'ido-completion-help            |
;;
;; Magic commands.
;; | C-b | 'ido-magic-backward-char |
;; | C-f | 'ido-magic-forward-char  |
;; | C-d | 'ido-magic-delete-char   |
;;
;; File and directory map
;; | C-x C-b                      | 'ido-enter-switch-buffer                 |
;; | C-x C-f                      | 'ido-fallback-command                    |
;; | C-x C-d                      | 'ido-enter-dired                         |
;; | Down                         | 'ido-next-match-dir                      |
;; | Up                           | 'ido-prev-match-dir                      |
;; | M-Up                         | 'ido-prev-work-directory                 |
;; | M-Down                       | 'ido-next-work-directory                 |
;; | Backspace                    | 'ido-delete-backward-updir               |
;; | Delete                       | 'ido-delete-backward-updir               |
;; | [remap delete-backward-char] | 'ido-delete-backward-updir) ; B          |
;; | [remap backward-kill-word]   | 'ido-delete-backward-word-updir)  ; M-DE |
;; | C-Backspace                  | 'ido-up-directory                        |
;; | C-l                          | 'ido-reread-directory                    |
;; | M-d                          | 'ido-wide-find-dir-or-delete-dir         |
;; | M-b                          | 'ido-push-dir                            |
;; | M-v                          | 'ido-push-dir-first                      |
;; | M-f                          | 'ido-wide-find-file-or-pop-dir           |
;; | M-k                          | 'ido-forget-work-directory               |
;; | M-m                          | 'ido-make-directory                      |
;; | M-n                          | 'ido-next-work-directory                 |
;; | M-o                          | 'ido-prev-work-file                      |
;; | M-C-o                        | 'ido-next-work-file                      |
;; | M-p                          | 'ido-prev-work-directory                 |
;; | M-s                          | 'ido-merge-work-directories              |
;;
;; File only map
;; | C-k | 'ido-delete-file-at-head                                         |
;; | C-o | 'ido-copy-current-word                                           |
;; | C-w | 'ido-copy-current-file-name (Insert file name of current buffer) |
;; | M-l | 'ido-toggle-literal                                              |
;;
;; Buffer map
;; | C-x C-f | 'ido-enter-find-file        |
;; | C-x C-b | 'ido-fallback-command       |
;; | C-k     | 'ido-kill-buffer-at-head    |
;; | C-o     | 'ido-toggle-virtual-buffers |
