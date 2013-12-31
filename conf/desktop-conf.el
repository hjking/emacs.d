;;
;; Filename: desktop-conf.el
;; Description: Setting for desktop.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2013-12-31 17:03:47
;; Reference: http://www.emacswiki.org/DeskTop
;;
(require 'desktop)
(if is-after-emacs-22
    (desktop-save-mode 1)
    (setq desktop-load-locked-desktop t)
    ;; (setq desktop-dirname "~/emacs.d/")
    (setq history-length 250)
    (setq desktop-dirname my-cache-dir)
    (setq destop-base-file-name (concat my-cache-dir "emacs.desktop"))
)
(if is-before-emacs-21
    (desktop-load-default)
    (setq history-length 250)
    (desktop-read)
    (setq desktop-enable t)
    (setq desktop-dirname my-cache-dir)
    (setq destop-base-file-name (concat my-cache-dir "emacs.desktop"))
)

(setq desktop-restore-eager 5)
(setq desktop-save (quote if-exists))

;; specify buffers which should not be saved, by name or by mode
(setq desktop-buffers-not-to-save
    (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
            "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
            "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)

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
