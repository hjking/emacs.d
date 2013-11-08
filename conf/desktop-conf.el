;;
;; Filename: desktop-conf.el
;; Description: Setting for desktop.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2012-10-27 21:59:00
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
(add-to-list 'desktop-globals-to-save 'file-name-history)

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
