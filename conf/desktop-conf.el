;;
;; Filename: desktop-conf.el
;; Description: Setting for desktop.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2014-01-02 12:35:28
;; Reference: http://www.emacswiki.org/DeskTop
;;

(message "%d: >>>>> Loading [ desktop ] Customizations ...." step_no)
(setq step_no (1+ step_no))

(require 'desktop)
(setq history-length 500)
;; use only one desktop
(setq desktop-dirname my-cache-dir)
(setq desktop-path (list desktop-dirname))
(setq desktop-base-file-name "emacs.desktop")
(setq desktop-file-name (concat my-cache-dir "emacs.desktop"))

(if is-after-emacs-22
    (desktop-save-mode 1)
    (setq desktop-load-locked-desktop t)
)
(if is-before-emacs-21
    (desktop-load-default)
    (desktop-read)
    (setq desktop-enable t)
)

(setq desktop-restore-eager 5)
(setq desktop-save 'if-exists)
  (defadvice desktop-read (around trace-desktop-errors)
    (let ((debug-on-error t))
      ad-do-it))

;; specify buffers which should not be saved, by name or by mode
(setq desktop-buffers-not-to-save
    (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
            "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
            "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'sr-mode)
(add-to-list 'desktop-modes-not-to-save 'sr-tree-mode)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (ido-last-directory-list  . 100)
                (ido-work-directory-list  . 100)
                (ido-work-file-list       . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (comint-input-ring        . 50)
                (shell-command-history    . 50)
                (evil-ex                  .100)
                desktop-missing-file-warning
                tags-file-name
                register-alist)))

;;
;; when start desktop, it looks a saved desktop file in current dir
;; `desktop-save: save desktop
;; `desktop-change-dir: save cueernt desktop
;; `desktop-revert: revert to perivous loaded desktop
;; `staet Emacs with '--no-desktop' to disable desktop package
;; `desktop-restore-eager: set number of files at startup
;; `desktop-clear: clean desktop
;; `desktop-clear-preserve-buffers-regexp: set to save which desktops
;;
;; Better ifea: use 'ibuffer' to kill no need buffer, then save desktop

;; remove desktop after it's been read
; (add-hook 'desktop-after-read-hook
;       '(lambda ()
;          ;; desktop-remove clears desktop-dirname
;          (setq desktop-dirname-tmp desktop-dirname)
;          (desktop-remove)
;          (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p desktop-file-name))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save (&optional noconfirm)
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if noconfirm
          (desktop-save-in-desktop-dir)
        ;; else
        (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save-in-desktop-dir)
          (message "Session not saved.")))
    (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
; (add-hook 'after-init-hook
;       '(lambda ()
;          (if (saved-session)
;          (if (y-or-n-p "Restore desktop? ")
;              (session-restore)))))
