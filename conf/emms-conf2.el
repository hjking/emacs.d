
;; Filename: emms-conf.el
;; Description: Setting for emms.el
;; Author: Hong Jin
;; Created: 2011-12-09 10:00
;; Last Updated: 2012-04-16 20:58:27
;;
(message ">>>>> Loading [ EMMS ] Customizations File ....")

;; set the location of the music player in Win
(when win32p
  (if (file-directory-p "D:/Tool/MPlayer")
      (add-to-list 'exec-path "D:/Tool/MPlayer"))
)

(require 'emms-setup)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-player-simple)
(require 'emms-player-mplayer)
(require 'emms-playlist-mode)
(require 'emms-info)
(require 'emms-cache)
(require 'emms-mode-line)
(require 'emms-playing-time)
(require 'emms-score)
(require 'emms-volume)
(require 'emms-playlist-sort)
(require 'emms-info-libtag)

(defadvice emms-play-directory-tree (after emms-random-play-1 activate)
  "This advice to make `emms-random' execute after emms-play-directory-tree"
  (emms-random))

(defadvice emms-play-directory-tree (after emms-playlist-sort-by-natural-order-1 activate)
  "This advice to make `emms-playlist-sort-by-natural-order' execute after emms-play-directory-tree"
  (emms-playlist-sort-by-natural-order))

(defadvice emms-history-load (after play-default activate)
  "If after `emms-history-load', get empty playlist,
play default music directory."
  (when (not emms-player-playing-p)
    (emms-play-now)
    (message "Emms history load failed, load default music directory...")))

(emms-standard)
(emms-default-players)
(emms-mode-line 1)
(emms-score 1)
(emms-playing-time 1)

(setq emms-score-file "~/.emacs.d/emms/scores")
;; show lyrics
(require 'emms-lyrics)
;; (emms-lyrics 1)
;; auto identify encode
(require 'emms-i18n)
;; auto save and import playlist
(require 'emms-history)
(emms-history-load)

(setq emms-player-mplayer-command-name "mplayer.exe")
;; (setq emms-player-mpg321-command-name "mpg123")

(setq emms-cache-file "~/.emacs.d/emms/emms-cache")
(setq emms-history-file "~/.emacs.d/emms/history")
(setq emms-playlist-buffer-name "EMMS Music Playlist")
(setq emms-playlist-default-major-mode 'emms-playlist-mode)
(setq emms-playlist-mode-window-width (* 0.3 (frame-width)))
;; 解析歌手和歌名
(setq emms-info-functions '(emms-info-libtag))
(add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
;; (add-to-list 'emms-info-functions 'kid-emms-info-simple)

;;
;; Mode line format
;; show info at mode-line
;; (setq emms-mode-line-mode-line-function 'bigclean-emms-mode-line-playlist-current)
(setq emms-mode-line-mode-line-function 'xwl-emms-mode-line-playlist-current)
(setq emms-mode-line-titlebar-function nil)

;;
;; Playlist buffer format
;; (setq emms-last-played-format-alist
;;       '(((emms-last-played-seconds-today) . "%a %H:%M")
;;         (604800                           . "%a %H:%M") ; this week
;;         ((emms-last-played-seconds-month) . "%d")
;;         ((emms-last-played-seconds-year)  . "%m/%d")
;;         (t                                . "%Y/%m/%d")))
(eval-after-load "emms"
  '(progn
     (setq xwl-emms-playlist-last-track nil)
     (setq xwl-emms-playlist-last-indent "\\")
     ;; (setq emms-track-description-function 'kid-emms-info-track-description)
     ;; (setq emms-track-description-function 'my-emms-info-track-description)
     (setq emms-track-description-function 'bigclean-emms-info-track-description)
     ;; (setq emms-track-description-function 'xwl-emms-track-description-function)

     ))


(setq emms-info-asynchronously nil)

(when win32p
    ;; use faster finding facility if you have GNU find
    (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-internal))
(when linuxp
    (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find))

;; My music location
(when win32p
  (setq emms-source-file-default-directory "E:/Users/X220_02/Music/iTunes/iTunes Media/Music")
)

;; Lyrics
(setq emms-lyrics-coding-system nil)     ;; let emacs to identify the encode of lyrics
;; (setq emms-lyrics-dir "/home/music/lyrics")

;; coding settings
;; (setq emms-info-mp3info-coding-system 'gbk)
(setq emms-cache-file-coding-system 'utf-8-emacs)
(setq emms-history-file-coding-system emms-cache-file-coding-system)
(setq emms-i18n-default-coding-system '(no-conversion . no-conversion))
;; (setq emms-i18n-default-coding-system '(utf-8 . utf-8))

(add-to-list 'file-coding-system-alist '("/[mM]usic/.*" gbk . gbk))

(add-hook 'emms-player-started-hook 'emms-show) ; show the coming song
(setq emms-show-format "Now Playing: %s")
(setq emms-repeat-playlist t)   ; repeat at the end

(setq emms-mode-line-format "[ %s "
      emms-playing-time-display-format "%s ]")
(setq global-mode-string
      '("" emms-mode-line-string " " emms-playing-time-string))

(setq emms-playing-time-style 'bar)

;;; Track Show Format (for playlist buffer)
;; (emms-playlist-selected-face ((t (:background "blue4" :foreground "Yellow"))))
;; (emms-playlist-track-face ((t nil)))

;; (when (fboundp 'emms-cache)
;;   (emms-cache 1))

(defun kid-emms-info-simple (track)
  "Get info from the filename.
mp3 标签的乱码问题总是很严重，幸好我系统里面的音乐文件
都放得比较有规律，所以我决定直接从文件名获取标签信息。"
  (when (eq 'file (emms-track-type track))
    (let ((regexp "/\\([^/]+\\)/\\([^/]+\\)\\.[^.]+$")
          (name (emms-track-name track)))
      (if (string-match regexp name)
          (progn
            (emms-track-set track 'info-artist (match-string 1 name))
            (emms-track-set track 'info-title (match-string 2 name)))
          (emms-track-set track
                          'info-title
                          (file-name-nondirectory name))))))


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
         "%-35s %-45s %-40s %5s:%-2s"
         (if artist artist "")
         (if title title "")
         (if album album "")
         (/ ptime 60)
         (% ptime 60)))))

(defun kid-emms-info-track-description (track)
  "Return a description of the current track."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (format "%-10s +| %s"
            (or artist
                "")
            title)))

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

(defun my-emms-google-for-lyrics ()
  (interactive)
  (browse-url
   (concat "http://www.google.com/search?q="
           (replace-regexp-in-string " +" "+"
                                     (concat "lyrics "
                                             (delete ?- (emms-track-description
                             (emms-playlist-current-selected-track))))))))


(defun xwl-emms-mode-line-playlist-current ()
  "Format the currently playing song."
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (name (emms-track-name track))
         (artist (emms-track-get track 'info-artist))
         (title (emms-track-get track 'info-title)))
    (concat emms-mode-line-icon-before-format
            (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
            (format "[ %s ]"
                    (cond
                     ((and artist title)
                      (concat artist " - " title))
                     (title
                      title)
                     ((eq type 'file)
                      (file-name-sans-extension (file-name-nondirectory name)))
                     (t
                      (concat (symbol-name type) ":" name)))))))


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

(defun emms-playlist-play-filename ()
  "Return the filename the current play."
  (cdr (assoc 'name (emms-playlist-current-selected-track))))

(defun emms-play-now()
  "Play default music directory."
  (interactive)
  (emms-play-directory-tree emms-source-file-default-directory))

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
(define-key emms-playlist-mode-map (kbd "<F4>") 'emms-play-directory)
(define-key emms-playlist-mode-map (kbd "<F5>") 'emms-add-directory-tree)
(define-key emms-playlist-mode-map (kbd "<F6>") 'emms-play-directory-tree)
(define-key emms-playlist-mode-map (kbd "<F7>") 'emms-previous)
(define-key emms-playlist-mode-map (kbd "<F8>") 'emms-next)
(define-key emms-playlist-mode-map (kbd "<F9>") 'my-emms-google-for-lyrics)
(define-key emms-playlist-mode-map (kbd "s-n") 'emms-next)
(define-key emms-playlist-mode-map (kbd "s-p") 'emms-previous)
(define-key emms-playlist-mode-map (kbd "s-s") 'emms-shuffle)

