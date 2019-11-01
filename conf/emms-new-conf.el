;; -*- lexical-binding: t -*-
;;
;; Filename: emms-conf.el
;; Description: Setting for emms.el
;; Author: Hong Jin
;; Created: 2011-12-09 10:00
;; Last Updated: 2014-01-02 10:57:12
;;

(use-package emms
  :load-path (lambda () (concat my-site-lisp-dir "emms/lisp/"))
  :commands (emms
             emms-history-load)
  ; :init
  :config

  (prefer-coding-system 'chinese-gbk)

  ;; ,----
  ;; | Dependency
  ;; `----

  ;; set the location of the music player in Win
  (when win32p
    ;; MPlayer
    (if (file-directory-p "D:/Tools/MPlayer")
        (add-to-list 'exec-path "D:/Tools/MPlayer")
      (message "*** Warning!! Please install MPlayer first!!"))
    ;; mp3info
    ; (if (file-directory-p "D:/Tools/mp3info")
    ;     (add-to-list 'exec-path "D:/Tools/mp3info")
    ;   (message "*** Warning!! Please install mp3info first!!"))
    ;; emms-print-metadata
    (setq my-emms-print-metadata-dir (concat my-emms-load-path "emms-print-metadata/"))
    (if (file-directory-p my-emms-print-metadata-dir)
      (add-to-list 'exec-path my-emms-print-metadata-dir)))

  (setq my-emms-conf-dir (concat my-cache-dir "emms/"))
  (unless (file-directory-p my-emms-conf-dir)
      (make-directory my-emms-conf-dir))

  ;; ,----
  ;; | Basic
  ;; `----

(require 'emms-setup)
(emms-standard)
(emms-default-players)
;; no cli volume setup tools in windows
;(require 'emms-volume)
(require 'emms-score)
(emms-score 1)
;; autodetect musci files id3 tags encodeing
(require 'emms-i18n)
;; auto-save and import last playlist
(require 'emms-history)

;; Using TagLib
(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

;; my customizable playlist format
; (defun bigclean-emms-info-track-description (track)
;   "Return a description of the current track."
;   (let ((artist (emms-track-get track 'info-artist))
;         (title (emms-track-get track 'info-title))
;         (album (emms-track-get track 'info-album))
;         (ptime (emms-track-get track 'info-playing-time)))
;     (if title
;         (format
;          "%-35s %-40s %-35s %5s:%-5s"
;          (if artist artist "")
;          (if title title "")
;          (if album album "")
;          (/ ptime 60)
;          (% ptime 60)))))
; (setq emms-track-description-function
;       'bigclean-emms-info-track-description)

;; format current track,only display title in mode line
; (defun bigclean-emms-mode-line-playlist-current ()
;   "Return a description of the current track."
;   (let* ((track (emms-playlist-current-selected-track))
;          (type (emms-track-type track))
;          (title (emms-track-get track 'info-title)))
;     (format "[ %s ]"
;             (cond ((and title)
;                    title)))))
; (setq emms-mode-line-mode-line-function
;       'bigclean-emms-mode-line-playlist-current)

;; coding settings
(setq emms-info-mp3info-coding-system 'gbk
      emms-cache-file-coding-system 'utf-8
      ;; emms-i18n-default-coding-system '(utf-8 . utf-8)
      )

  ;; load history
  ; (emms-history-load)

  :bind (
         :map emms-playlist-mode-map
          ("SPC"  . emms-pause)
          ("s"    . emms-stop)
          ("t"    . emms-start)
          ("n"    . emms-next)
          ("p"    . emms-previous)
          ("l"    . emms-play-playlist)
          ("h"    . emms-shuffle)
          (">"    . emms-seek-forward)
          ("<"    . emms-seek-backward)
          ("+"    . emms-volume-mode-plus)
          ("-"    . emms-volume-mode-minus)
          ("r"    . emms-toggle-repeat-track)
          ("R"    . emms-toggle-repeat-playlist)
          ("S u"  . emms-score-up-file-on-line)
          ("S d"  . emms-score-down-file-on-line)
          ("S o"  . emms-score-show-file-on-line)
          ("S l"  . emms-score-less-tolerant)
          ("S m"  . emms-score-more-tolerant)
          ("S t"  . emms-score-set-tolerance)
          ("S s"  . emms-score-show-playing)
          ("<F4>" . emms-play-directory)
          ("<F5>" . emms-add-directory-tree)
          ("<F6>" . emms-play-directory-tree)
          ; ("<F7>" . emms-previous)
          ; ("<F8>" . emms-next)
         )

  )

(provide 'emms-new-conf)