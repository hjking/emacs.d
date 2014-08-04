
;; Filename: emms-conf.el
;; Description: Setting for emms.el
;; Author: Hong Jin
;; Created: 2011-12-09 10:00
;; Last Updated: 2014-01-02 10:57:12
;;
(message "%d: >>>>> Loading [ EMMS ] Customizations File ...." step_no)
(setq step_no (1+ step_no))

;; set the location of the music player in Win
(when win32p
  (if (file-directory-p "D:/Tools/MPlayer")
      (add-to-list 'exec-path "D:/Tools/MPlayer")
    (message "*** Warning!! Please install MPlayer first!!"))
  (setq emms-print-metadata-dir (concat my-emms-load-path "emms-print-metadata/"))
  (if (file-directory-p emms-print-metadata-dir)
    (add-to-list 'exec-path emms-print-metadata-dir))
  )

(require 'emms-setup)
(require 'emms-player-mplayer)
(require 'emms-streams)
(emms-all)
(emms-default-players)
; (require 'emms-source-file)
; (require 'emms-source-playlist)
; (require 'emms-player-simple)
; (require 'emms-player-mplayer)
; (require 'emms-playlist-mode)
; (require 'emms-info)
; (require 'emms-cache)
; (require 'emms-mode-line)
; (require 'emms-playing-time)
; (require 'emms-score)
; (require 'emms-volume)
; (require 'emms-playlist-sort)
(require 'emms-info-libtag)

(emms-mode-line 1)

(emms-score 1)

(emms-playing-time 1)

;; show lyrics
(require 'emms-lyrics)
;; (emms-lyrics 1)
;; auto identify encode
(require 'emms-i18n)
;; auto save and import playlist
(require 'emms-history)

;;(setq emms-player-mplayer-command-name "mplayer.exe")

(setq my-emms-conf-dir (concat my-emacs-dir "emms/"))
(unless (file-directory-p my-emms-conf-dir)
    (make-directory my-emms-conf-dir))

;; (setq emms-score-file "~/.emacs.d/emms/emms-scores")
(setq emms-score-file (concat my-emms-conf-dir "emms-scores"))
;; (setq emms-cache-file "~/.emacs.d/emms/emms-cache")
(setq emms-cache-file (concat my-emms-conf-dir "emms-cache"))
;; (setq emms-history-file "~/.emacs.d/emms/emms-history")
(setq emms-history-file (concat my-emms-conf-dir "emms-history"))
(setq emms-playlist-buffer-name "EMMS Music Playlist")
;; 设定EMMS用播放列表的主模式
(setq emms-playlist-default-major-mode 'emms-playlist-mode)
;; (setq emms-playlist-mode-window-width (* 0.3 (frame-width)))
(setq emms-info-functions '(emms-info-libtag))
;; 设定音轨初始化信息
(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
;; (add-to-list 'emms-info-functions 'kid-emms-info-simple)

;;
;; Mode line format
;; show info at mode-line
(setq emms-mode-line-mode-line-function 'bigclean-emms-mode-line-playlist-current)
;; (setq emms-mode-line-mode-line-function 'xwl-emms-mode-line-playlist-current)
;; (setq emms-mode-line-titlebar-function nil)

;;
;; Playlist buffer format
;; (setq emms-track-description-function 'my-emms-info-track-description)
(setq emms-track-description-function 'bigclean-emms-info-track-description)
;; (setq emms-last-played-format-alist
;;       '(((emms-last-played-seconds-today) . "%a %H:%M")
;;         (604800                           . "%a %H:%M") ; this week
;;         ((emms-last-played-seconds-month) . "%d")
;;         ((emms-last-played-seconds-year)  . "%m/%d")
;;         (t                                . "%Y/%m/%d")))
;; (eval-after-load "emms"
;;   '(progn
;;      (setq xwl-emms-playlist-last-track nil)
;;      (setq xwl-emms-playlist-last-indent "\\")
;;      ;; (setq emms-track-description-function 'kid-emms-info-track-description)
;;      ;; (setq emms-track-description-function 'my-emms-info-track-description)
;;      (setq emms-track-description-function 'bigclean-emms-info-track-description)
;;      ;; (setq emms-track-description-function 'xwl-emms-track-description-function)
;;
;;      ))

;; 关闭EMMS信息异步模式, 不然会处错
(setq emms-info-asynchronously nil)

(when win32p
    ;; use faster finding facility if you have GNU find
    (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-internal))
(when linuxp
    (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find))

;; My music location
(when win32p
  (setq emms-source-file-default-directory "F:/Music")
)
(when linuxp
  (setq emms-source-file-default-directory "~/Music")
)

;; playlist
;; 设置播放列表用自然的方法排序: 艺术家 -> 专辑 -> 序号
(setq emms-playlist-sort-function 'emms-playlist-sort-by-natural-order)
;; (setq my-emms-playlist (concat emms-source-file-default-directory "playlist"))
(setq my-emms-playlist (concat my-emms-conf-dir "playlist"))
;; Save current playlist to file before exit
;; (add-hook 'kill-emacs-hook (lambda()
;;                              (set-buffer emms-playlist-buffer)
;;                              (write-region (point-min) (point-max) my-emms-playlist nil)))
;;; Track Show Format (for playlist buffer)
;; (emms-playlist-selected-face ((t (:background "blue4" :foreground "Yellow"))))
;; (emms-playlist-track-face ((t nil)))

;; Lyrics
(setq my-emms-lyrics (concat emms-source-file-default-directory "lyrics"))
(setq emms-lyrics-coding-system nil)     ;; let emacs to identify the encode of lyrics
(setq emms-lyrics-dir my-emms-lyrics)
(setq emms-lyrics-display-format "%s")

;; coding settings
(setq emms-info-mp3info-coding-system 'utf-8)
(setq emms-cache-file-coding-system 'utf-8-emacs)
(setq emms-history-file-coding-system emms-cache-file-coding-system)
;; (setq emms-i18n-default-coding-system '(no-conversion . no-conversion))
;; (setq emms-i18n-default-coding-system '(utf-8 . utf-8))

;; (add-to-list 'file-coding-system-alist '("/[mM]usic/.*" gbk . gbk))

(add-hook 'emms-player-started-hook 'emms-show) ; show the coming song
(setq emms-show-format "Now Playing: %s")
;;
;; 修复该死的播放完后的BUG
(setq emms-player-next-function 'emms-next)
;; 设定EMMS启动列表循环播放
(setq emms-repeat-playlist t)   ; repeat at the end

(setq emms-mode-line-format "[ %s "
      emms-playing-time-display-format "%s ]")
(setq global-mode-string
      '("" emms-mode-line-string " " emms-playing-time-string))

(setq emms-playing-time-style 'bar)

;;>>>>>>
;(add-hook 'emms-player-finished-hook 'emms-random)          ;当播放完当前的歌曲时随机选择下一首歌曲
(emms-player-set emms-player-mplayer 'regex "\\.ogg\\|\\.mp3\\|\\.wav\\|\\.mpg\\|\\.mpeg\\|\\.wmv\\|\\.wma\\|\\.mov\\|\\.avi\\|\\.divx\\|\\.ogm\\|\\.asf\\|\\.mkv\\|http://\\|mms://\\|\\.rm\\|\\.rmvb\\|\\.mp4\\|\\.flac\\|\\.vob\\|\\.m4a\\|\\.ape\\|\\.mpc")

;;>>>>>>
(setq emms-stream-info-format-string "NS: %s"
      emms-stream-default-action "play"
      emms-stream-popup-default-height 120)

;; (when (fboundp 'emms-cache)
;;   (emms-cache 1))

;; (defun kid-emms-info-simple (track)
;;   "Get info from the filename.
;; mp3 标签的乱码问题总是很严重，幸好我系统里面的音乐文件
;; 都放得比较有规律，所以我决定直接从文件名获取标签信息。"
;;   (when (eq 'file (emms-track-type track))
;;     (let ((regexp "/\\([^/]+\\)/\\([^/]+\\)\\.[^.]+$")
;;           (name (emms-track-name track)))
;;       (if (string-match regexp name)
;;           (progn
;;             (emms-track-set track 'info-artist (match-string 1 name))
;;             (emms-track-set track 'info-title (match-string 2 name)))
;;           (emms-track-set track
;;                           'info-title
;;                           (file-name-nondirectory name))))))


;; format current track,only display title in mode line
(defun bigclean-emms-mode-line-playlist-current ()
  "Return a description of the current track."
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (title (emms-track-get track 'info-title)))
    (format "[ %s ]"
            (cond ((and title)
                   title)))))

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

;; (defun kid-emms-info-track-description (track)
;;   "Return a description of the current track."
;;   (let ((artist (emms-track-get track 'info-artist))
;;         (title (emms-track-get track 'info-title)))
;;     (format "%-10s +| %s"
;;             (or artist
;;                 "")
;;             title)))

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

;; (defun my-emms-google-for-lyrics ()
;;   (interactive)
;;   (browse-url
;;    (concat "http://www.google.com/search?q="
;;            (replace-regexp-in-string " +" "+"
;;                                      (concat "lyrics "
;;                                              (delete ?- (emms-track-description
;;                              (emms-playlist-current-selected-track))))))))


;; (defun xwl-emms-mode-line-playlist-current ()
;;   "Format the currently playing song."
;;   (let* ((track (emms-playlist-current-selected-track))
;;          (type (emms-track-type track))
;;          (name (emms-track-name track))
;;          (artist (emms-track-get track 'info-artist))
;;          (title (emms-track-get track 'info-title)))
;;     (concat emms-mode-line-icon-before-format
;;             (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
;;             (format "[ %s ]"
;;                     (cond
;;                      ((and artist title)
;;                       (concat artist " - " title))
;;                      (title
;;                       title)
;;                      ((eq type 'file)
;;                       (file-name-sans-extension (file-name-nondirectory name)))
;;                      (t
;;                       (concat (symbol-name type) ":" name)))))))


;;    (defun xwl-emms-track-description-function (track)
;;    "Return a description of the current track."
;;    (let* ((name (emms-track-name track))
;;          (type (emms-track-type track))
;;          (short-name (file-name-nondirectory name))
;;          (play-count (or (emms-track-get track 'play-count) 0))
;;          (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
;;          (empty "..."))
;;     (prog1
;;         (case (emms-track-type track)
;;           ((file url)
;;            (let* ((artist (or (emms-track-get track 'info-artist) empty))
;;                   (year (emms-track-get track 'info-year))
;;                   (playing-time (or (emms-track-get track 'info-playing-time) 0))
;;                   (min (/ playing-time 60))
;;                   (sec (% playing-time 60))
;;                   (album (or (emms-track-get track 'info-album) empty))
;;                   (tracknumber (emms-track-get track 'info-tracknumber))
;;                   (short-name (file-name-sans-extension
;;                                (file-name-nondirectory name)))
;;                   (title (or (emms-track-get track 'info-title) short-name))
;;
;;                   ;; last track
;;                   (ltrack xwl-emms-playlist-last-track)
;;                   (lartist (or (and ltrack (emms-track-get ltrack 'info-artist))
;;                                empty))
;;                   (lalbum (or (and ltrack (emms-track-get ltrack 'info-album))
;;                               empty))
;;
;;                   (same-album-p (and (not (string= lalbum empty))
;;                                      (string= album lalbum))))
;;              (format "%10s  %3d   %-20s%-60s%-35s%-15s%s"
;;                      (emms-last-played-format-date last-played)
;;                      play-count
;;                      artist
;;
;;                      ;; Combine indention, tracknumber, title.
;;                      (concat
;;                       (if same-album-p ; indention by album
;;                           (setq xwl-emms-playlist-last-indent
;;                                 (concat " " xwl-emms-playlist-last-indent))
;;                         (setq xwl-emms-playlist-last-indent "\\")
;;                         "")
;;                       (if (and tracknumber ; tracknumber
;;                                (not (zerop (string-to-number tracknumber))))
;;                           (format "%02d." (string-to-number tracknumber))
;;                         "")
;;                       title        ; title
;;                       )
;;
;;                      ;; album
;;                      (cond ((string= album empty) empty)
;;                            ;; (same-album-p "  ")
;;                            (t (concat "《" album "》")))
;;
;;                      (or year empty)
;;                      (if (or (> min 0)  (> sec 0))
;;                          (format "%02d:%02d" min sec)
;;                        empty))))
;;           ((url)
;;            (concat (symbol-name type) ":" name))
;;           (t
;;            (format "%-3d%s"
;;                    play-count
;;                    (concat (symbol-name type) ":" name))))
;;       (setq xwl-emms-playlist-last-track track))))

(defun my-emms-play-default ()             ; default play set
  (interactive)
  (emms-play-directory emms-source-file-default-directory)
  (process-kill-without-query (get-process "emms-player-mpg321-remote-proc")))
                                        ; no pop window when exit


;;
(message ">>>>> Loading [ EMMS Key Binding ] ....")
;; global key-map
;; all global keys prefix is C-c e
;; compatible with emms-playlist mode keybindings
;; you can view emms-playlist-mode.el to get details about
;; emms-playlist mode keys map
(global-set-key (kbd "C-c e s") 'emms-stop)
(global-set-key (kbd "C-c e SPC") 'emms-pause)
(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e f") 'emms-show)
(global-set-key (kbd "C-c e >") 'emms-seek-forward)
(global-set-key (kbd "C-c e <") 'emms-seek-backward)
;; these keys maps were derivations of above keybindings
(global-set-key (kbd "C-c e x") 'emms-start)
(global-set-key (kbd "C-c e g") 'emms-playlist-mode-go)
(global-set-key (kbd "C-c e t") 'emms-play-directory-tree)
(global-set-key (kbd "C-c e h") 'emms-shuffle)
(global-set-key (kbd "C-c e e") 'emms-play-file)
(global-set-key (kbd "C-c e l") 'emms-play-playlist)
(global-set-key (kbd "C-c e r") 'emms-toggle-repeat-track)
(global-set-key (kbd "C-c e R") 'emms-toggle-repeat-playlist)
(global-set-key (kbd "C-c e u") 'emms-score-up-playing)
(global-set-key (kbd "C-c e d") 'emms-score-down-playing)
(global-set-key (kbd "C-c e o") 'emms-score-show-playing)
(global-set-key (kbd "C-c e a") 'emms-add-directory-tree)
;; (global-set-key (kbd "<f5>") 'emms-playlist-mode-go)

;; playlist-mode-map
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
(define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
;; (define-key emms-playlist-mode-map (kbd "<right>") (lambda () (interactive) (emms-seek +10)))
;; (define-key emms-playlist-mode-map (kbd "<left>") (lambda () (interactive) (emms-seek -10)))
;; (define-key emms-playlist-mode-map (kbd "<up>") (lambda () (interactive) (emms-seek +60)))
;; (define-key emms-playlist-mode-map (kbd "<down>") (lambda () (interactive) (emms-seek -60)))
(define-key emms-playlist-mode-map (kbd "S u") 'emms-score-up-file-on-line)
(define-key emms-playlist-mode-map (kbd "S d") 'emms-score-down-file-on-line)
(define-key emms-playlist-mode-map (kbd "S o") 'emms-score-show-file-on-line)
(define-key emms-playlist-mode-map (kbd "S l") 'emms-score-less-tolerant)
(define-key emms-playlist-mode-map (kbd "S m") 'emms-score-more-tolerant)
(define-key emms-playlist-mode-map (kbd "S t") 'emms-score-set-tolerance)
(define-key emms-playlist-mode-map (kbd "S s") 'emms-score-show-playing)
(define-key emms-playlist-mode-map (kbd "<F3>") 'my-emms-play-default)
(define-key emms-playlist-mode-map (kbd "<F4>") 'emms-play-directory)
(define-key emms-playlist-mode-map (kbd "<F5>") 'emms-add-directory-tree)
(define-key emms-playlist-mode-map (kbd "<F6>") 'emms-play-directory-tree)
(define-key emms-playlist-mode-map (kbd "<F7>") 'emms-previous)
(define-key emms-playlist-mode-map (kbd "<F8>") 'emms-next)
;; (define-key emms-playlist-mode-map (kbd "<F9>") 'my-emms-google-for-lyrics)
(define-key emms-playlist-mode-map (kbd "s-n") 'emms-next)
(define-key emms-playlist-mode-map (kbd "s-p") 'emms-previous)
(define-key emms-playlist-mode-map (kbd "s-s") 'emms-shuffle)

(emms-history-load)
