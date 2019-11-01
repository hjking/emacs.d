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
    ; mp3info
    (if (file-directory-p "D:/Tools/mp3info")
        (add-to-list 'exec-path "D:/Tools/mp3info")
      (message "*** Warning!! Please install mp3info first!!"))
    ;; emms-print-metadata
    ; (setq my-emms-print-metadata-dir (concat my-emms-load-path "emms-print-metadata/"))
    ; (if (file-directory-p my-emms-print-metadata-dir)
    ;   (add-to-list 'exec-path my-emms-print-metadata-dir))
    )

  (setq my-emms-conf-dir (concat my-cache-dir "emms/"))
  (unless (file-directory-p my-emms-conf-dir)
      (make-directory my-emms-conf-dir))

  ;; ,----
  ;; | Basic
  ;; `----

  ;;; Quick-Setup
  ;; All what we need is the following block
  (require 'emms-setup)
  ;; Loads all the stable features which come with the Emms distribution.
  (emms-all)
  ;; Set emms-player-list to emms-setup-default-player-list.
  (emms-default-players)

  ; ; (require 'emms-score)
  ;; (setq emms-score-file "~/.emacs.d/emms/emms-scores")
  (setq emms-score-file (concat my-emms-conf-dir "emms-scores"))

  ; (require 'emms-cache)
  ;; (setq emms-cache-file "~/.emacs.d/emms/emms-cache")
  (setq emms-cache-file (concat my-emms-conf-dir "emms-cache"))

  ;; emms-history: auto save and import playlist
  ; (require 'emms-history)
  ;; (setq emms-history-file "~/.emacs.d/emms/emms-history")
  (setq emms-history-file (concat my-emms-conf-dir "emms-history"))

  (setq emms-playlist-buffer-name "*Music*")

  ;; make emms-playlist buffer the default Emms playlist mode
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)
  ;; (setq emms-playlist-mode-window-width (* 0.3 (frame-width)))

  ; (require 'emms-source-file)
  (when win32p
      ;; use faster finding facility if you have GNU find
      (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-internal))
  (when linuxp
      (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find))

  ; My music location
  (when win32p
    (setq emms-source-file-default-directory "F:/Media/Music")
  )
  (when linuxp
    (setq emms-source-file-default-directory "~/Music")
  )

  ; (setq emms-mode-line-format "[ %s "
  ;       emms-playing-time-display-format "%s ]")
  ; (setq global-mode-string
  ;       '("" emms-mode-line-string " " emms-playing-time-string))

  ; (setq emms-playing-time-style 'bar) ;; like [===>......]

  ;;
  ;; playlist
  ;;
  ;; playlist emms sort
  ;; 设置播放列表用自然的方法排序: 艺术家 -> 专辑 -> 序号
  (setq emms-playlist-sort-function 'emms-playlist-sort-by-natural-order)
  ; ;; (setq my-emms-playlist (concat emms-source-file-default-directory "playlist"))
  ; (setq my-emms-playlist (concat my-emms-conf-dir "playlist"))

  ;; (setq emms-i18n-default-coding-system '(no-conversion . no-conversion))
  ;; (setq emms-i18n-default-coding-system '(utf-8 . utf-8))

  ;; (add-to-list 'file-coding-system-alist '("/[mM]usic/.*" gbk . gbk))

  ;; emms-show: Describe the current Emms track in the minibuffer
  ;; "%s" is replaced by what emms-track-description-function returns for the currently playing track
  (setq emms-show-format "Now Playing: %s")
  (add-hook 'emms-player-started-hook 'emms-show) ; show the coming song

  ;;
  ;; Playlist buffer format
  ;; (setq emms-track-description-function 'my-emms-info-track-description)
  ; (setq emms-track-description-function 'bigclean-emms-info-track-description)
  ;; (setq emms-last-played-format-alist
  ;;       '(((emms-last-played-seconds-today) . "%a %H:%M")
  ;;         (604800                           . "%a %H:%M") ; this week
  ;;         ((emms-last-played-seconds-month) . "%d")
  ;;         ((emms-last-played-seconds-year)  . "%m/%d")
  ;;         (t                                . "%Y/%m/%d")))

  ;; my customizable playlist format
  (defun bigclean-emms-info-track-description (track)
    "Return a description of the current track."
    (let ((artist (emms-track-get track 'info-artist))
          (title (emms-track-get track 'info-title))
          (album (emms-track-get track 'info-album))
          (ptime (emms-track-get track 'info-playing-time)))
      (if title
          (format
           "%-25s %-45s %-35s %5s:%-2s"
           (if artist artist "")
           (if title title "")
           (if album album "")
           (/ ptime 60)
           (% ptime 60)))))

  (defun my-emms-info-track-description (track)
    "Return a description of the current track."
    (if (and (emms-track-get track 'info-artist)
             (emms-track-get track 'info-title))
        (let ((pmin (emms-track-get track 'info-playing-time-min))
              (psec (emms-track-get track 'info-playing-time-sec))
              (ptot (emms-track-get track 'info-playing-time))
              (art  (emms-track-get track 'info-artist))
              (tit  (emms-track-get track 'info-title)))
          (cond ((and pmin psec) (format "%s - %s [%02s:%02s]" art tit pmin psec))
                (ptot (format  "%s - %s [%02s:%02s]" art tit (/ ptot 60) (% ptot 60)))
                (t (emms-track-simple-description track))))))

  ;;
  ;; 修复该死的播放完后的BUG
  (setq emms-player-next-function 'emms-next)

  ;; 设定EMMS启动列表循环播放
  (setq emms-repeat-playlist t)   ; repeat at the end

  ;;>>>>>>
  (setq emms-stream-info-format-string "NS: %s"
        emms-stream-default-action "play"
        emms-stream-popup-default-height 120)

  ;;
  ;; Emms Mode Line
  ;; show info at mode-line
  ; (require 'emms-mode-line)
  ; (require 'emms-playing-time)
  (emms-mode-line 1)
  (emms-playing-time 1)

  ; (setq emms-mode-line-mode-line-function 'bigclean-emms-mode-line-playlist-current)

  ;; format current track, only display title in mode line
  (defun bigclean-emms-mode-line-playlist-current ()
    "Return a description of the current track."
    (let* ((track (emms-playlist-current-selected-track))
           (type (emms-track-type track))
           (title (emms-track-get track 'info-title)))
      (format "[ %s ]"
              (cond ((and title)
                     title)))))

  ;; score
  (emms-score 1)

  ;; Track Information
  ; (setq emms-info-mp3info-coding-system 'gbk)
  (setq emms-cache-file-coding-system 'utf-8)

  ;; 关闭EMMS信息异步模式, 不然会处错
  (setq emms-info-asynchronously nil)

  ;; Using TagLib
  ; (require 'emms-info-libtag)
  ; (setq emms-info-functions '(emms-info-libtag))

  ;; show lyrics
  ; (require 'emms-lyrics)
  ;; (emms-lyrics 1)

  ;; autodetect musci files id3 tags encodeing
  ; (require 'emms-i18n)

  (setq emms-history-file-coding-system emms-cache-file-coding-system)

  ;; 设定音轨初始化信息
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)

  ;debug players
  (emms-player-for '(*track* (type . file)
                             (name . "myfile.pls")))

  (define-emms-simple-player mplayer '(file url)
        (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                      ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                      ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
        "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")

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

(provide 'emms-new-conf1)